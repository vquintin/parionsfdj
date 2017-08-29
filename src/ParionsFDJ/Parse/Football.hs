{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ParionsFDJ.Parse.Football
  (
  ) where

import Data.Text (Text, splitOn)
import qualified HBet.Bet as HB
import qualified HBet.Football as FB
import qualified HBet.Types as TY
import qualified ParionsFDJ.JSON.Event as EV
import qualified ParionsFDJ.JSON.Formule as F
import qualified ParionsFDJ.JSON.Outcome as OC
import ParionsFDJ.Parse.Outcome
import ParionsFDJ.Parse.Parsable

instance Parsable EV.Event [HB.Choice FB.Football ()] where
  parseData e = do
    let tag = ()
    eventInfo <- parseData e
    betTypesAndOdds <- undefined
    return $ do
      (betType, choiceOdd) <- betTypesAndOdds
      return HB.Choice {..}

instance Parsable F.Formule [(HB.BetType FB.Football, Double)] where
  parseData f = zip <$> parseData f <*> parseData f

instance Parsable EV.Event (HB.Match FB.Football) where
  parseData e = HB.Match <$> parseData e <*> parseData e

instance Parsable EV.Event (HB.Lineup FB.Football) where
  parseData e = undefined

instance Parsable EV.Event (HB.Competition FB.Football) where
  parseData e = parseData $ EV.competition e

instance Parsable Text (HB.Competition FB.Football) where
  parseData t =
    case t of
      "Ligue 1" -> return $ FB.ChD1 TY.France
      "Ligue 2" -> return $ FB.ChD2 TY.France
      "Bundesliga 2" -> return $ FB.ChD2 TY.Germany
      _ -> extractWithCountry t
    where
      extractWithCountry s =
        case splitOn " " s of
          ["Ch.D1", country] -> FB.ChD1 <$> parseData country
          ["Ch.D2", country] -> FB.ChD2 <$> parseData country
          _ -> Left $ "Unknown competition" ++ show s

instance Parsable Text TY.Country where
  parseData t =
    case t of
      "Belgique" -> return TY.Belgium
      "Bulgarie" -> return TY.Bulgaria
      "Chili" -> return TY.Chile
      "Croatie" -> return TY.Croatia
      "France" -> return TY.France
      "Allemagne" -> return TY.Germany
      "Irlande" -> return TY.Ireland
      "Japon" -> return TY.Japan
      "Mexique" -> return TY.Mexico
      "Pologne" -> return TY.Poland
      "Slovénie" -> return TY.Slovenia
      "Roumanie" -> return TY.Romania
      _ -> Left $ "Unknown Country " ++ show t

instance Parsable F.Formule [HB.BetType FB.Football] where
  parseData f =
    case F.marketTypeGroup f of
      "Mi-Temps" ->
        (fmap . fmap)
          FB.HalfTimeWinOrDraw
          (traverse (parseData . OC.label) (F.outcomes f))

instance Parsable F.Formule [Double] where
  parseData f = traverse (parseData . OC.cote) (F.outcomes f)

instance Parsable (EV.Nullable [a]) [a] where
  parseData EV.Null = Right []
  parseData (EV.NotNull xs) = Right xs

instance (Parsable [a] [b]) => Parsable (EV.Nullable [a]) [b] where
  parseData nxs =
    case nxs of
      EV.Null -> Right []
      EV.NotNull xs -> parseData xs
