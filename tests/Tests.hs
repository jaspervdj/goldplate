{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson            as A
import           Data.ByteString.Char8 ()
import qualified Data.List             as List
import           Goldplate             (Assert)
import           System.Environment    (getArgs)
import           System.Exit           (exitWith)
import           System.Process        (system)

-- See https://github.com/jaspervdj/goldplate/issues/22
testAssertMultipleDiscriminator :: IO ()
testAssertMultipleDiscriminator =
    case A.eitherDecode bytes :: Either String (Assert String) of
        Left err | "discriminator" `List.isInfixOf` err -> pure ()
        _ -> fail $
            "testAssertMultipleDiscriminator: expected discriminator error"
  where
    bytes = "{\"exit_code\": 0, \"stdout\": \"stdout.txt\"}"

main :: IO ()
main = do
  testAssertMultipleDiscriminator
  args <- getArgs
  exitWith =<< do
    system $ unwords $ "goldplate tests" : args
