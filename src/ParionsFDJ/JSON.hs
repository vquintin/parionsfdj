module ParionsFDJ.JSON
  (
  ) where

import qualified Data.Time as T

data Formule = Formule
  { competition :: String
  , competitionId :: Int
  , count :: Int
  , end :: T.UTCTime
  , formuleLabel :: String
  , outcomes :: [Outcome]
  } deriving (Eq, Show)

data Outcome = Outcome
  { cote :: Double
  , outcomeLabel :: String
  , pos :: Int
  , status :: Int
  , trend :: Trend
  , winner :: Bool
  } deriving (Eq, Show)

data Trend
  = Down
  | Nil
  | Up
  deriving (Eq, Show)
