module Text.Splice
    ( SpliceEnv
    , MissingEnvVar (..)
    , splice
    ) where

import           Control.Exception (Exception)
import qualified Data.List         as List
import           Data.Typeable     (Typeable)

-- | Environment for splicing in things.
type SpliceEnv = [(String, String)]

data MissingEnvVar = MissingEnvVar String
    deriving (Typeable)

instance Show MissingEnvVar where
    show (MissingEnvVar k) = "Missing environment variable: " ++ k

instance Exception MissingEnvVar

-- | Splice in a string with "Hello ${FOO}" syntax.
splice :: SpliceEnv -> String -> Either MissingEnvVar String
splice env = go
  where
    go str = case break (== '$') str of
        -- Splice
        (xs, '$' : '{' : ys) -> case break (== '}') ys of
            (key, '}' : zs) -> case lookup key env of
                Nothing  -> Left $ MissingEnvVar key
                Just val -> fmap ((xs ++ val) ++) (go zs)
            (_, _) -> fmap ((xs ++ "${") ++) (go ys)
        -- Escape
        (xs, '$' : '$' : ys) ->
            let (dollars, zs) = break (== '{') ys in
            if all (== '$') dollars && "{" `List.isPrefixOf` zs
                then (xs ++) . (dollars ++) . ("${" ++) <$> go (drop 1 zs)
                else (xs ++) . ("$$" ++) <$> go ys
        (xs, []) -> Right xs
        (xs, (y : ys)) -> (xs ++) . (y :) <$> (go ys)
