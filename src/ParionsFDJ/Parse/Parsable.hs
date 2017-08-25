{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ParionsFDJ.Parse.Parsable
  ( Parsable(..)
  ) where

import Data.Text (Text)
import qualified Data.Text.Read as TR

class Parsable from to where
  parseData :: from -> Either String to

instance Parsable a a where
  parseData = Right

instance Parsable Text Int where
  parseData t = TR.decimal t >>= (\(r, _) -> Right r)
