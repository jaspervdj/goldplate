{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main
    ( main
    ) where

import           Control.Applicative       ((<|>))
import qualified Control.Concurrent.Async  as Async
import qualified Control.Concurrent.MVar   as MVar
import           Control.Exception         (finally, throwIO)
import           Control.Monad             (forM, forM_, mzero)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Encode.Pretty  as Aeson.Pretty
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Foldable             as F
import           Data.Function             (on)
import qualified Data.HashMap.Strict       as HMS
import qualified Data.IORef                as IORef
import qualified Data.List                 as List
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Data.Version              (showVersion)
import qualified Options.Applicative       as OA
import           Paths_goldplate           (version)
import qualified System.Directory          as Dir
import           System.Environment        (getEnvironment)
import           System.Exit               (ExitCode (..), exitFailure)
import qualified System.FilePath           as FP
import qualified System.FilePath.Glob      as Glob
import qualified System.IO                 as IO
import qualified System.Process            as Process
import qualified Text.Regex.PCRE.Simple    as Pcre
import           Text.Splice

--------------------------------------------------------------------------------

-- | This is a little helper type that we use when we either support multiple
-- things (e.g. lines of stdin) or a single thing (e.g. a single string of
-- stdin).
data Multiple a = Multiple [a] | Single a
    deriving (Foldable, Functor, Traversable)

instance A.FromJSON a => A.FromJSON (Multiple a) where
    parseJSON v = (Multiple <$> A.parseJSON v) <|> (Single <$> A.parseJSON v)

multipleToList :: Multiple a -> [a]
multipleToList = F.toList

--------------------------------------------------------------------------------

-- | A specification that we parse from a JSON file.
-- The type parameter indicates the fields that we allow splicing over.
data Spec a = Spec
    { specInputFiles :: !(Maybe a)
    , specCommand    :: !a
    , specArguments  :: ![a]
    , specStdin      :: !(Maybe (Multiple a))
    , specEnv        :: ![(a, a)]
    , specAsserts    :: ![Assert a]
    } deriving (Foldable, Functor, Traversable)

instance A.FromJSON (Spec String) where
    parseJSON = A.withObject "FromJSON Spec" $ \o -> Spec
        <$> o A..:? "input_files"
        <*> o A..:  "command"
        <*> o A..:? "arguments" A..!= []
        <*> o A..:? "stdin"
        <*> (maybe [] HMS.toList <$> o A..:? "environment")
        <*> o A..:  "asserts"

--------------------------------------------------------------------------------

-- | Post processing of stdout or created files.
type PostProcess = [PostProcessStep]

data PostProcessStep
    = PrettifyJsonStep
    | ReplaceStep !Pcre.Regex !T.Text

instance A.FromJSON PostProcessStep where
    parseJSON = \case
        A.String "prettify_json" -> pure PrettifyJsonStep
        A.Object o -> ReplaceStep
            <$> (do
                    p <- o A..: "pattern"
                    either (fail . show) return (Pcre.compile copts eopts p))
            <*> o A..: "replacement"
        _ -> mzero
      where
        copts = Pcre.optionUtf8 <> Pcre.optionMultiline
        eopts = mempty

postProcess :: PostProcess -> B.ByteString -> B.ByteString
postProcess ps bs0 = List.foldl' (flip postProcessStep) bs0 ps

postProcessStep :: PostProcessStep -> B.ByteString -> B.ByteString
postProcessStep PrettifyJsonStep bs = maybe bs
    (BL.toStrict . Aeson.Pretty.encodePretty' prettyConfig)
    (A.decodeStrict bs :: Maybe A.Value)
  where
    prettyConfig = Aeson.Pretty.defConfig
        { Aeson.Pretty.confIndent  = (Aeson.Pretty.Spaces 2)
        , Aeson.Pretty.confCompare = compare
        }

postProcessStep (ReplaceStep regex replacement) bs =
    either (const bs) T.encodeUtf8 .
    Pcre.replaceAll regex replacement $ T.decodeUtf8 bs

--------------------------------------------------------------------------------

-- | Asserts that can happen after an execution.
data Assert a
    = ExitCodeAssert !Int
    | StdoutAssert
        { stdoutFilePath    :: !a
        , stdoutPostProcess :: !PostProcess
        }
    | StderrAssert
        { stderrFilePath    :: !a
        , stderrPostProcess :: !PostProcess
        }
    | CreatedFileAssert
        { createdFilePath        :: !a
        , createdFileContents    :: !(Maybe a)
        , createdFilePostProcess :: !PostProcess
        }
    | CreatedDirectoryAssert
        { createdDirectoryPath   :: !a
        }
    deriving (Foldable, Functor, Traversable)

instance A.FromJSON a => A.FromJSON (Assert a) where
    parseJSON = A.withObject "FromJSON Assert" $ \o ->
        (ExitCodeAssert <$> o A..: "exit_code") <|>
        (StdoutAssert <$> o A..: "stdout" <*> pp o) <|>
        (StderrAssert <$> o A..: "stderr" <*> pp o) <|>
        (CreatedFileAssert
            <$> o A..: "created_file" <*> o A..:? "contents" <*> pp o) <|>
        (CreatedDirectoryAssert <$> o A..: "created_directory")
      where
        pp o = maybe [] multipleToList <$> o A..:? "post_process"

describeAssert :: Assert a -> String
describeAssert (ExitCodeAssert     _)     = "exit_code"
describeAssert (StdoutAssert       _ _)   = "stdout"
describeAssert (StderrAssert       _ _)   = "stderr"
describeAssert (CreatedFileAssert  _ _ _) = "created_file"
describeAssert (CreatedDirectoryAssert _) = "created_directory"

--------------------------------------------------------------------------------

data TapUnit = TapUnit Bool String [String] deriving (Show)

data Logger = Logger
    { logDebug    :: [String] -> IO ()
    , logError    :: [String] -> IO ()
    , logNumUnits :: Int -> IO ()
    , logTapUnit  :: TapUnit -> IO ()
    }

makeLogger :: Bool -> IO Logger
makeLogger verbose = do
    lock <- MVar.newMVar ()
    let writeLines h ls = MVar.withMVar lock $ \() -> mapM_ (IO.hPutStrLn h) ls
    return Logger
        { logDebug    = if verbose then writeLines IO.stderr else \_ -> pure ()
        , logError    = writeLines IO.stderr
        , logNumUnits = \n -> writeLines IO.stdout ["1.." ++ show n]
        , logTapUnit  = writeLines IO.stdout . formatTapUnit
        }
  where
    formatTapUnit (TapUnit ok header info) =
        ((if ok then "ok " else "not ok ") ++ header) :
        map ("     " ++) (concatMap lines info)

--------------------------------------------------------------------------------

-- | A plain 'Spec' parsed from a JSON file usually gives us one more or
-- executions of a process.  This contains more info than a plain 'Spec'.
data Execution = Execution
    { executionSpec      :: Spec String
    , executionInputFile :: Maybe FilePath
    , executionSpecPath  :: FilePath
    , executionSpecName  :: String
    , executionDirectory :: FilePath
    }

specExecutions :: FilePath -> Spec String -> IO [Execution]
specExecutions specPath spec = do
    let (specDirectory, specBaseName) = FP.splitFileName specPath
        specName                      = FP.dropExtension specBaseName

    -- Compute initial environment to get input files.
    env0 <- getEnvironment
    let env1 =
            List.nubBy ((==) `on` fst) $
                ("GOLDPLATE_NAME", specName) :
                ("GOLDPLATE_FILE", specBaseName) :
                ("GOLDPLATE_BASENAME", specBaseName) :
                specEnv spec ++ env0

    -- Get a list of concrete input files (a list maybes).
    concreteInputFiles <- case specInputFiles spec of
        Nothing    -> return [Nothing]
        Just glob0 -> do
            glob <- hoistEither $ splice env1 glob0
            inputFiles <- Dir.withCurrentDirectory specDirectory $ do
                matches <- globCurrentDir glob
                length matches `seq` return matches
            return (map (Just . FP.normalise) inputFiles)

    -- Create an execution for every concrete input.
    forM concreteInputFiles $ \mbInputFile -> do
        -- Extend environment.
        let env2 = case mbInputFile of
                Nothing        -> env1
                Just inputFile ->
                    ("GOLDPLATE_INPUT_FILE", inputFile) :
                    ("GOLDPLATE_INPUT_NAME", FP.dropExtension inputFile) :
                    ("GOLDPLATE_INPUT_BASENAME", snd $ FP.splitFileName inputFile) :
                    env1

        -- Return execution after doing some splicing.
        hoistEither $ do
            spec' <- traverse (splice env2) spec
            pure Execution
                { executionSpec      = spec' {specEnv = env2}
                , executionInputFile = mbInputFile
                , executionSpecPath  = specPath
                , executionSpecName  = specName
                , executionDirectory = specDirectory
                }
  where
    hoistEither :: Either MissingEnvVar a -> IO a
    hoistEither = either throwIO return

executionHeader :: Execution -> String
executionHeader execution =
    executionSpecPath execution ++
    case executionInputFile execution of
        Nothing -> ": "
        Just fp -> " (" ++ fp ++ "): "

--------------------------------------------------------------------------------

data Env = Env
    { envLogger     :: !Logger
    , envDiff       :: !Bool
    , envPrettyDiff :: !Bool
    , envFix        :: !Bool
    }

data ExecutionResult = ExecutionResult
    { erExitCode :: !ExitCode
    , erStdout   :: !B.ByteString
    , erStderr   :: !B.ByteString
    } deriving (Show)

runExecution
    :: Env -> Execution -> IO ExecutionResult
runExecution env execution@Execution {..} = do
    let Spec {..} = executionSpec
    logDebug (envLogger env) [executionHeader execution ++ "running..."]

    -- Create a "CreateProcess" description.
    let createProcess = (Process.proc specCommand specArguments)
            { Process.env     = Just specEnv
            , Process.cwd     = Just executionDirectory
            , Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

    -- Actually run the process.
    logDebug (envLogger env) [executionHeader execution ++
        specCommand ++ " " ++ unwords specArguments]
    (Just hIn, Just hOut, Just hErr, hProc) <-
        Process.createProcess createProcess

    let writeStdin = (`finally` IO.hClose hIn) $ case specStdin of
            Nothing              -> pure ()
            Just (Single str)    -> IO.hPutStr hIn str
            Just (Multiple strs) -> mapM_ (IO.hPutStrLn hIn) strs
    Async.withAsync writeStdin $ \_ ->
        Async.withAsync (B.hGetContents hOut) $ \outAsync ->
        Async.withAsync (B.hGetContents hErr) $ \errAsync ->
        Async.withAsync (Process.waitForProcess hProc) $ \exitAsync -> do

        -- Get output.
        !exitCode  <- Async.wait exitAsync
        !actualOut <- Async.wait outAsync
        !actualErr <- Async.wait errAsync
        logDebug (envLogger env)
            [ executionHeader execution ++ "finished"
            , "exit code: " ++ show exitCode
            , "stdout:", show actualOut
            , "stderr:", show actualErr
            ]
        pure ExecutionResult
            { erExitCode = exitCode
            , erStdout   = actualOut
            , erStderr   = actualErr
            }

data AssertResult = AssertResult
    { arAssert  :: Assert String
    , arOk      :: Bool
    , arMessage :: [String]
    , arDebug   :: [String]
    }

-- | Check a single assertion.
runAssert
    :: Env -> Execution -> ExecutionResult -> Assert String -> IO AssertResult
runAssert env Execution {..} ExecutionResult {..} assert =
    case assert of
        ExitCodeAssert expectedExitCode ->
            let actualExitCode = case erExitCode of
                    ExitSuccess   -> 0
                    ExitFailure c -> c
                success = expectedExitCode == actualExitCode in
            pure $ AssertResult assert success
                ["expected " ++ show expectedExitCode ++
                    " but got " ++ show actualExitCode | not success]
                []

        StdoutAssert {..} -> checkAgainstFile
            (inExecutionDir stdoutFilePath) stdoutPostProcess erStdout

        StderrAssert {..} -> checkAgainstFile
            (inExecutionDir stderrFilePath) stderrPostProcess erStderr

        CreatedFileAssert {..} -> do
            let path = inExecutionDir createdFilePath
            exists <- Dir.doesFileExist path
            case exists of
                False -> pure $ AssertResult assert False
                    [createdFilePath ++ " was not created"] []
                True -> case createdFileContents of
                    Nothing           -> pure $ AssertResult assert True [] []
                    Just expectedPath -> do
                        !actual <- readFileOrEmpty path
                        ar <- checkAgainstFile
                            (inExecutionDir expectedPath)
                            createdFilePostProcess actual
                        Dir.removeFile path
                        pure ar
                            { arDebug = arDebug ar ++
                                ["removed " ++ createdFilePath]
                            }

        CreatedDirectoryAssert {..} -> do
            let path = inExecutionDir createdDirectoryPath
            exists <- Dir.doesDirectoryExist path
            case exists of
                False -> pure $ AssertResult assert False
                    [createdDirectoryPath ++ " was not created"] []
                True -> do
                    Dir.removeDirectoryRecursive path
                    pure $ AssertResult assert True []
                        ["removed " ++ createdDirectoryPath]
  where
    inExecutionDir :: FilePath -> FilePath
    inExecutionDir fp =
        if FP.isAbsolute fp then fp else executionDirectory FP.</> fp

    checkAgainstFile
        :: FilePath -> PostProcess -> B.ByteString -> IO AssertResult
    checkAgainstFile expectedPath processor actual0 = do
        expected <- readFileOrEmpty expectedPath
        let !actual1 = postProcess processor actual0
            success = actual1 == expected
            shouldFix = envFix env && not success

            diff :: [Diff [String]] = either (const []) id $ do
                expected' <- T.unpack <$> T.decodeUtf8' expected
                actual1'  <- T.unpack <$> T.decodeUtf8' actual1
                return $
                    getGroupedDiff
                        (lines expected')
                        (lines actual1')

        pure AssertResult
            { arAssert  = assert
            , arOk      = success
            , arMessage = concat $
                [ ["does not match"] | not success ] ++
                [ [ "expected:"
                  , show expected
                  , "actual:"
                  , show actual1
                  ]
                | not success && envDiff env
                ] ++
                [ [ "diff:", ppDiff diff ]
                | not success && envPrettyDiff env
                ] ++
                [ ["fixed " ++ expectedPath] | shouldFix ]
            , arDebug = []
            }


--------------------------------------------------------------------------------

-- | Read a file if it exists, otherwise pretend it's empty.
readFileOrEmpty :: FilePath -> IO B.ByteString
readFileOrEmpty fp = do
    exists <- Dir.doesFileExist fp
    if exists then B.readFile fp else return B.empty

-- | Recursively finds all '.goldplate' files in bunch of files or directories.
findSpecs :: [FilePath] -> IO [FilePath]
findSpecs fps = fmap concat $ forM fps $ \fp -> do
    isDir <- Dir.doesDirectoryExist fp
    case isDir of
        False -> return [fp]
        True  -> Glob.globDir1 (Glob.compile "**/*.goldplate") fp

-- | Perform a glob match in the current directory.
--
-- This is a drop-in replacement for `glob` from the `Glob` library, which has a
-- an annoying tendency to return absolute file paths.
globCurrentDir :: String -> IO [FilePath]
globCurrentDir pattern =
    map dropLeadingDot <$> Glob.globDir1 (Glob.compile pattern) "."
  where
    dropLeadingDot fp0 = case break FP.isPathSeparator fp0 of
        (".", fp1) -> drop 1 fp1
        _          -> fp0

--------------------------------------------------------------------------------

-- | Command-line options.
data Options = Options
    { oPaths      :: [FilePath]
    , oVerbose    :: Bool
    , oDiff       :: Bool
    , oPrettyDiff :: Bool
    , oFix        :: Bool
    , oJobs       :: Int
    }

parseOptions :: OA.Parser Options
parseOptions = Options
    <$> OA.some (OA.strArgument (
            OA.metavar "PATH" <>
            OA.help    "Test files/directories"))
    <*> OA.switch (
            OA.short   'v' <>
            OA.help    "Print debug info")
    <*> OA.switch (
            OA.long    "diff" <>
            OA.help    "Show differences in files")
    <*> OA.switch (
            OA.long    "pretty-diff" <>
            OA.help    "Show differences in files, output in patch format")
    <*> OA.switch (
            OA.long    "fix" <>
            OA.help    "Attempt to fix broken tests")
    <*> OA.option OA.auto (
            OA.long    "jobs" <>
            OA.short   'j'    <>
            OA.value   1      <>
            OA.help    "Number of worker jobs")

parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parseOptions) $
    OA.fullDesc <>
    OA.header ("goldplate v" <> showVersion version)

--------------------------------------------------------------------------------

-- | Spawn a worker thread that takes workloads from a shared pool.
worker
    :: IORef.IORef [a]                         -- ^ Ref to a pool of work
    -> (a -> IO ())                            -- ^ Worker function
    -> IO ()
worker pool f = do
    mbWorkload <- IORef.atomicModifyIORef' pool $ \case
        []       -> ([], Nothing)
        (x : xs) -> (xs, Just x)
    case mbWorkload of
        Nothing       -> return ()
        Just workload -> f workload >> worker pool f

--------------------------------------------------------------------------------

main :: IO ()
main = do
    options <- OA.execParser parserInfo
    env     <- Env
        <$> makeLogger (oVerbose options)
        <*> pure (oDiff options)
        <*> pure (oPrettyDiff options)
        <*> pure (oFix options)

    -- Find all specs and decode them.
    specPaths <- findSpecs (oPaths options)
    specs     <- forM specPaths $ \specPath -> do
        !errOrSpec <- A.eitherDecodeStrict <$> B.readFile specPath
        case errOrSpec of
            Right !spec -> return (specPath, spec)
            Left  !err  -> do
                logError (envLogger env)
                    [specPath ++ ": could not parse JSON: " ++ err]
                exitFailure

    -- Each spec might produce a number of executions.  We can't really
    -- parallelize this because 'specExecutions' needs to change the working
    -- directory all the time and that might mess with our tests.
    let numSpecs = length specs
    logDebug (envLogger env) ["Found " ++ show numSpecs ++ " specs"]
    executions <- fmap concat $ forM specs $
        \(specPath, spec) -> specExecutions specPath spec

    -- Create a pool full of executions.
    let numJobs       = oJobs options
        numAsserts    = sum $
            map (length . specAsserts . executionSpec) executions
    logNumUnits (envLogger env) numAsserts
    pool <- IORef.newIORef executions

    -- Spawn some workers to run the executions.
    Async.replicateConcurrently_ numJobs $ worker pool $ \execution -> do
        executionResult <- runExecution env execution
        forM_ (zip [1 :: Int ..] (specAsserts $ executionSpec execution)) $
            \(i, assert) -> do
                assertResult <- runAssert env execution executionResult assert
                let tapUnit = TapUnit
                        (arOk assertResult)
                        (executionHeader execution ++ show i ++ " " ++
                            describeAssert assert)
                        (arMessage assertResult)
                logTapUnit (envLogger env) tapUnit

    -- Report summary.
    {-
    asserts       <- IORef.readIORef (envCountAsserts  env)
    failures      <- IORef.readIORef (envCountFailures env)
    if failures == 0
        then
            envLogger env Message [
                "Ran " ++ show numSpecs ++ " specs, " ++
                show numExecutions ++ " executions, " ++
                show asserts ++ " asserts, all A-OK!"]
        else do
            envLogger env Error [
                "Ran " ++ show numSpecs ++ " specs, " ++
                show numExecutions ++ " executions, " ++
                show asserts ++ " asserts, " ++ show failures ++ " failed."]
            exitFailure
    -}
