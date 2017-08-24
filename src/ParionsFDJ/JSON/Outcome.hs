{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module ParionsFDJ.JSON.Outcome
  ( Outcome(..)
  , Winner(..)
  ) where

import Data.Aeson (FromJSON)
import qualified Data.Aeson as AE
import qualified Data.Scientific as SC
import Data.Text (Text)
import GHC.Generics (Generic)

data Outcome = Outcome
  { cote :: Text
  , label :: Text
  , pos :: Text
  , status :: Int
  , trend :: Text
  , winner :: Winner
  } deriving (Eq, Show, Generic, FromJSON)

data Winner
  = Bool Bool
  | Int Int
  deriving (Eq, Show)

instance AE.FromJSON Winner where
  parseJSON (AE.Bool b) = return $ Bool b
  parseJSON o = AE.withScientific "winner int" f o
    where
      f = g . SC.toBoundedInteger
      g (Just i) = return $ Int i
      g Nothing = fail $ "Not an integer" ++ show o
