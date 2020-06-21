{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Aeson.Extended
    ( module Data.Aeson
    , Multiple (..)
    , multipleToList
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson

data Multiple a
    = Multiple [a]
    | Single a
    deriving (Foldable, Functor, Traversable)

instance FromJSON a => FromJSON (Multiple a) where
    parseJSON val = (Multiple <$> parseJSON val) <|> (Single <$> parseJSON val)

multipleToList :: Multiple a -> [a]
multipleToList (Multiple xs) = xs
multipleToList (Single x)    = [x]
