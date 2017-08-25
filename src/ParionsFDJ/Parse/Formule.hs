{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module ParionsFDJ.Parse.Formule
  (
  ) where

import Data.Text (Text, splitOn)
import Data.Time (UTCTime)
import qualified ParionsFDJ.JSON.Formule as F
import ParionsFDJ.Parse.Outcome (POutcome)
import ParionsFDJ.Parse.Parsable

data PFormule = PFormule
  { competitionID :: Int
  , end :: UTCTime
  , eventInfo :: EventInfo
  , marketID :: Int
  , marketType :: MarketTypeGroup
  , marketTypeID :: Int
  , outcomes :: [POutcome]
  } deriving (Eq, Show)

newtype EventInfo =
  Football FootballEvent
  deriving (Eq, Show)

instance Parsable F.Formule EventInfo where
  parseData f = Football <$> parseData f

data FootballEvent =
  Match FootballCompetition
        MatchLineup
  deriving (Eq, Show)

instance Parsable F.Formule FootballEvent where
  parseData f = do
    comp <- parseData $ F.competition f
    lineup <- parseData $ F.label f
    return $ Match comp lineup

data MatchLineup =
  MkMatchLineUp Text
                Text
  deriving (Eq, Show)

instance Parsable Text MatchLineup where
  parseData t =
    case splitOn "-" t of
      [a, b] -> return $ MkMatchLineUp a b
      _ -> Left $ "Can't parse match line-up" ++ show t

data FootballCompetition
  = ChD1 Country
  | ChD2 Country
  deriving (Eq, Show)

instance Parsable Text FootballCompetition where
  parseData t =
    case t of
      "Ligue 1" -> return $ ChD1 France
      "Ligue 2" -> return $ ChD2 France
      "Bundesliga 2" -> return $ ChD2 Germany
      _ -> extractWithCountry t
    where
      extractWithCountry s =
        case splitOn " " s of
          ["Ch.D1", country] -> ChD1 <$> parseData country
          ["Ch.D2", country] -> ChD2 <$> parseData country
          _ -> Left $ "Unknown competition" ++ show s

{- Country -}
data Country
  = Belgium
  | Bulgaria
  | Chile
  | Croatia
  | France
  | Germany
  | Ireland
  | Japan
  | Mexico
  | Poland
  | Slovenia
  | Romania
  deriving (Eq, Show)

instance Parsable Text Country where
  parseData t =
    case t of
      "Belgique" -> return Belgium
      "Bulgarie" -> return Bulgaria
      "Chili" -> return Chile
      "Croatie" -> return Croatia
      "France" -> return France
      "Allemagne" -> return Germany
      "Irlande" -> return Ireland
      "Japon" -> return Japan
      "Mexique" -> return Mexico
      "Pologne" -> return Poland
      "Slovénie" -> return Slovenia
      "Roumanie" -> return Romania
      _ -> Left $ "Unknown Country " ++ show t

data MarketTypeGroup =
  HalfTime
  deriving (Eq, Show)
