{-# LANGUAGE OverloadedStrings #-}

module ParionsFDJ.JSON
  ( Trend(..)
  , Outcome(..)
  ) where

import qualified Data.Aeson as AE
import Data.Aeson ((.:))
import qualified Data.Text as TE
import qualified Data.Text.Read as TR
import qualified Data.Time as TI

data Event = Event
  { eventCompetition :: String
  , eventCompetitionID :: Int
  , eventCount :: Int
  , eventEnd :: Int
  , eventID :: Int
  , eventType :: String
  , formules :: [Formule]
  , eventSportID :: Int
  , urlStats :: String
  } deriving (Eq, Show)

data Formule = Formule
  { competition :: String
  , competitionId :: Int
  , count :: Int
  , end :: TI.UTCTime
  , formuleEventID :: Int
  , index :: Int
  , formuleLabel :: String
  , marketID :: Int
  , marketType :: String
  , marketTypeGroup :: String
  , marketTypeID :: Int
  , outcomes :: [Outcome]
  , sportID :: Int
  } deriving (Eq, Show)

{- Outcome -}
data Outcome = Outcome
  { cote :: Double
  , outcomeLabel :: String
  , pos :: Int
  , status :: Int
  , trend :: Trend
  , winner :: Bool
  } deriving (Eq, Show)

instance AE.FromJSON Outcome where
  parseJSON =
    AE.withObject "outcome" $ \o ->
      Outcome <$> (readDoubleWithComma <$> o .: "cote") <*> o .: "label" <*>
      (readIntAsText <$> o .: "pos") <*>
      o .: "status" <*>
      o .: "trend" <*>
      o .: "winner"

readDoubleWithComma :: TE.Text -> Double
readDoubleWithComma = read . sanitize . TE.unpack
  where
    sanitize =
      map
        (\c ->
           if c == ','
             then '.'
             else c)

readIntAsText :: TE.Text -> Int
readIntAsText = read . TE.unpack

{- Trend -}
data Trend
  = Down
  | Nil
  | Up
  deriving (Eq, Show)

instance AE.FromJSON Trend where
  parseJSON =
    AE.withText "trend" $ \t ->
      case t of
        "-1" -> return Down
        "0" -> return Nil
        "1" -> return Up
        _ -> fail $ "Unknown trend " ++ show t
