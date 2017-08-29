{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ParionsFDJ.Parse
  (
  ) where

import qualified HBet.BetSum as HBS
import qualified ParionsFDJ.JSON.Event as EV
import ParionsFDJ.Parse.Football
import ParionsFDJ.Parse.Parsable

instance Parsable EV.Event [HBS.ChoiceSum ()] where
  parseData ev =
    case EV.sportId ev of
      "100" -> (fmap . fmap) HBS.Football (parseData ev)
      _ -> Left $ "Unknown sport with id: " ++ show (EV.sportId ev)
