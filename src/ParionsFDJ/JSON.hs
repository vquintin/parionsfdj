{-# LANGUAGE OverloadedStrings #-}

module ParionsFDJ.JSON
  ( getEvents
  ) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Network.HTTP.Simple as H
import ParionsFDJ.JSON.Event (Event)

getEvents :: IO (Either String [Event])
getEvents = eitherDecode <$> getJSON

getJSON :: IO ByteString
getJSON = H.getResponseBody <$> getResp
  where
    getResp =
      H.httpLBS "https://www.pointdevente.parionssport.fdj.fr/api/1n2/offre"
