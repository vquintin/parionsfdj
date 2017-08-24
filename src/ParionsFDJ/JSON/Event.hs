{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}

module ParionsFDJ.JSON.Event
  ( Event(..)
  , Nullable(..)
  ) where

import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson as AE
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import ParionsFDJ.JSON.Formule (Formule)

data Event = Event
  { competition :: Text
  , competitionId :: Text
  , count :: Int
  , end :: UTCTime
  , eventId :: Text
  , eventType :: Text
  , formules :: Nullable [Formule]
  , sportId :: Text
  , urlStats :: Text
  } deriving (Eq, Show, Generic, FromJSON)

data Nullable a
  = NotNull a
  | Null

deriving instance (Show a) => Show (Nullable a)

deriving instance (Eq a) => Eq (Nullable a)

instance (FromJSON a) => FromJSON (Nullable a) where
  parseJSON AE.Null = return Null
  parseJSON o = NotNull <$> parseJSON o
