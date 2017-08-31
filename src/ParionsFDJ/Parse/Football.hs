{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParionsFDJ.Parse.Football
  (
  ) where

import Control.Monad (join)
import Data.Text (Text, pack, splitOn, unpack)
import qualified HBet.Bet as HB
import qualified HBet.Football as FB
import qualified HBet.Types as TY
import qualified ParionsFDJ.JSON.Event as EV
import qualified ParionsFDJ.JSON.Formule as F
import qualified ParionsFDJ.JSON.Outcome as OC
import ParionsFDJ.Parse.Outcome
import ParionsFDJ.Parse.Parsable
import Text.Read (readEither)
import qualified Text.Regex.TDFA as RE

instance Parsable EV.Event [HB.Choice FB.Football ()] where
  parseData e = do
    let tag = ()
    eventInfo <- parseData e
    betTypesAndOdds <- (go . nullableToList . EV.formules) e
    return $ do
      (betType, choiceOdd) <- betTypesAndOdds
      return HB.Choice {..}
    where
      go :: [F.Formule] -> Either String [(HB.BetType FB.Football, Double)]
      go fs = concat <$> sequence (fmap parseData fs)

instance Parsable F.Formule [(HB.BetType FB.Football, Double)] where
  parseData f = zip <$> parseData f <*> parseData f

instance Parsable EV.Event (HB.Match FB.Football) where
  parseData e = HB.Match <$> parseData e <*> parseData e

instance Parsable EV.Event (HB.Lineup FB.Football) where
  parseData e =
    case EV.formules e of
      EV.NotNull (f:_) ->
        case splitOn "-" (F.label f) of
          [a, b] -> Right $ FB.Lineup a b
          _ -> Left $ "Can't parse match line up: " ++ show (F.label f)
      _ -> Left "No formula in event"

instance Parsable EV.Event (HB.Competition FB.Football) where
  parseData e = parseData $ EV.competition e

instance Parsable Text (HB.Competition FB.Football) where
  parseData t =
    case t of
      "Ligue 1" -> return $ FB.ChD1 TY.France
      "Ligue 2" -> return $ FB.ChD2 TY.France
      "Bundesliga 1" -> return $ FB.ChD1 TY.Germany
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
  parseData form =
    case F.marketTypeGroup form of
      "Mi-Temps" -> go a form
      "Score exact" -> go b form
      "MT/FM" -> go c form
      "Double chance" -> go d form
      "Plus/Moins" ->
        let (_ :: String, _ :: String, _ :: String, [goals :: String]) =
              (unpack . F.marketType) form RE.=~
              ("Plus/Moins ([0-9]+,5).*" :: String)
            result = go <$> (e <$> (parseData . pack) goals) <*> pure form
        in join result
      "Handicap" ->
        let (_ :: String, _ :: String, _ :: String, [h1 :: String, h2]) =
              (unpack . F.marketType) form RE.=~
              ("Handicap \\[([0-9]+):([0-9]+)\\]" :: String)
            hcap = (-) <$> readEither h1 <*> readEither h2
            result = go <$> (f <$> hcap) <*> pure form
        in join result
      _ -> Left $ "Unknown market type group: " ++ show (F.marketTypeGroup form)
    where
      go h formule = traverse (h . OC.label) (F.outcomes formule)
      a :: Text -> Either String (HB.BetType FB.Football)
      a t = FB.HalfTimeWinOrDraw <$> parseData t
      b :: Text -> Either String (HB.BetType FB.Football)
      b t =
        case splitOn " - " t of
          [a, b] -> FB.ExactScore <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse score: " ++ show t
      c :: Text -> Either String (HB.BetType FB.Football)
      c t =
        case splitOn "/" t of
          [a, b] -> FB.HTFTWinOrDraw <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse HTFT: " ++ show t
      d :: Text -> Either String (HB.BetType FB.Football)
      d t =
        case splitOn "/" t of
          [a, b] -> FB.DoubleChance <$> parseData a <*> parseData b
          _ -> Left $ "Can't parse double chance: " ++ show t
      e :: Double -> Text -> Either String (HB.BetType FB.Football)
      e goals t =
        case t of
          "Plus" -> return $ FB.NumberOfGoals GT goals
          "Moins" -> return $ FB.NumberOfGoals LT goals
          _ -> Left $ "Can't parse plus/moins: " ++ show t
      f :: Int -> Text -> Either String (HB.BetType FB.Football)
      f hcap t = FB.Handicap <$> pure hcap <*> parseData t

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

nullableToList :: EV.Nullable [a] -> [a]
nullableToList nas =
  case nas of
    EV.Null -> []
    EV.NotNull as -> as
