{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings,
  RecordWildCards #-}

module ParionsFDJ.Parse.Outcome
  ( POutcome(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified ParionsFDJ.JSON.Outcome as OC
import ParionsFDJ.Parse.Parsable (Parsable(..))

data POutcome = POutcome
  { cote :: Double
  , label :: Text
  , pos :: Int
  , trend :: Trend
  } deriving (Eq, Show)

instance Parsable OC.Outcome POutcome where
  parseData oc = do
    cote <- parseData $ OC.cote oc
    let label = OC.label oc
    pos <- parseData $ OC.pos oc
    trend <- parseData $ OC.trend oc
    return POutcome {..}

instance Parsable Text Double where
  parseData t = do
    let sane = sanitize t
    (r, _) <- TR.double sane
    Right r
    where
      sanitize :: Text -> Text
      sanitize =
        T.map
          (\c ->
             if c == ','
               then '.'
               else c)

data Trend
  = Down
  | Nil
  | Up
  deriving (Eq, Show)

instance Parsable Text Trend where
  parseData "-1" = Right Down
  parseData "0" = Right Nil
  parseData "1" = Right Up
  parseData t = Left $ "Unknown trend " ++ show t
