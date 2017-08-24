{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ParionsFDJ.JSON.Formule
  ( Formule(..)
  , MarketTypeID(..)
  ) where

import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson as AE
import Data.Scientific as SC
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import ParionsFDJ.JSON.Outcome (Outcome)

data Formule = Formule
  { competition :: Text
  , competitionId :: Text
  , end :: UTCTime
  , eventId :: Text
  , index :: Text
  , label :: Text
  , marketId :: Text
  , marketType :: Text
  , marketTypeGroup :: Text
  , marketTypeId :: MarketTypeID
  , outcomes :: [Outcome]
  , sportId :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data MarketTypeID
  = AsInt Int
  | AsText Text
  deriving (Eq, Show)

instance FromJSON MarketTypeID where
  parseJSON (AE.String t) = return $ AsText t
  parseJSON o = AE.withScientific "marketTypeId int" f o
    where
      f = g . SC.toBoundedInteger
      g (Just i) = return $ AsInt i
      g Nothing = fail $ "Not an integer" ++ show o
