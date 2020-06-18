module Text.Regex.PCRE.Simple
    ( CompileOptions
    , optionUtf8
    , optionMultiline

    , ExecOptions

    , I.Regex
    , compile
    , replaceAll
    ) where

import           Data.Bifunctor         (first, second)
import           Data.Bits              ((.|.))
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import           System.IO.Unsafe       (unsafePerformIO)
import qualified Text.Regex.PCRE.Text   as I

newtype CompileOptions = CompileOptions I.CompOption

instance Semigroup CompileOptions where
    CompileOptions x <> CompileOptions y = CompileOptions (x .|. y)

instance Monoid CompileOptions where
    mempty = CompileOptions I.compBlank

optionUtf8 :: CompileOptions
optionUtf8 = CompileOptions I.compUTF8

optionMultiline :: CompileOptions
optionMultiline = CompileOptions I.compMultiline

newtype ExecOptions = ExecOptions I.ExecOption

instance Semigroup ExecOptions where
    ExecOptions x <> ExecOptions y = ExecOptions (x .|. y)

instance Monoid ExecOptions where
    mempty = ExecOptions I.execBlank

compile :: CompileOptions -> ExecOptions -> T.Text -> Either String I.Regex
compile (CompileOptions compOption) (ExecOptions execOption) = first snd .
    unsafePerformIO . I.compile compOption execOption

replaceAll :: I.Regex -> T.Text -> T.Text -> Either String T.Text
replaceAll regex replacement =
    second (TL.toStrict . TLB.toLazyText) . unsafePerformIO . go mempty
  where
    go acc text = do
        match <- I.regexec regex text
        case match of
            Left err -> pure . Left $ show err
            Right Nothing -> pure $ Right acc
            Right (Just (pre, _, post, _)) ->
                go (acc <> TLB.fromText pre <> TLB.fromText replacement) post
