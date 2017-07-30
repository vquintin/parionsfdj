{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParionsFDJ.JSON
  ( Trend(..)
  , Winner(..)
  , Outcome(..)
  , Formule(..)
  , Event(..)
  , Sport(..)
  , MarketType(..)
  , OutcomeType(..)
  , OutcomeWinner(..)
  , OutcomeExactScore(..)
  , OutcomeHTFT(..)
  , OutcomeMoreLess(..)
  , MatchLineUp(..)
  , parseBetJSON
  ) where

import Control.Applicative ((<|>))
import qualified Data.Aeson as AE
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BS
import qualified Data.Maybe as MY
import qualified Data.Scientific as SC
import qualified Data.Text as TE
import qualified Data.Text.Read as TR
import qualified Data.Time as TI

parseBetJSON :: BS.ByteString -> [Event]
parseBetJSON s = MY.catMaybes $ maybeEvents s
  where
    maybeEvents :: BS.ByteString -> [Maybe Event]
    maybeEvents s = concat $ f <$> AE.decode s
    f :: [AT.Value] -> [Maybe Event]
    f = fmap (AT.parseMaybe AE.parseJSON)

data Event = Event
  { eventCompetition :: String
  , eventCompetitionID :: Int
  , count :: Int
  , eventEnd :: TI.UTCTime
  , eventID :: Int
  , eventType :: String
  , formules :: [Formule]
  , eventSportID :: Sport
  , urlStats :: String
  } deriving (Eq, Show)

instance AE.FromJSON Event where
  parseJSON =
    AE.withObject "event" $ \o -> do
      eventCompetition <- o .: "competition"
      eventCompetitionID <- readIntAsText <$> o .: "competitionId"
      count <- o .: "count"
      eventEnd <- o .: "end"
      eventID <- readIntAsText <$> o .: "eventId"
      eventType <- o .: "eventType"
      formules <- MY.fromMaybe [] <$> o .:? "formules"
      eventSportID <- o .: "sportId"
      urlStats <- o .: "urlStats"
      return Event {..}

{- Formule -}
data Formule = Formule
  { competition :: String
  , competitionID :: Int
  , end :: TI.UTCTime
  , formuleEventID :: Int
  , index :: Int
  , formuleLabel :: MatchLineUp
  , marketID :: Int
  , marketType :: MarketType
  , marketTypeGroup :: String
  , marketTypeID :: Int
  , outcomes :: [Outcome]
  , sportID :: Sport
  } deriving (Eq, Show)

instance AE.FromJSON Formule where
  parseJSON =
    AE.withObject "formule" $ \o -> do
      competition <- o .: "competition"
      competitionID <- readIntAsText <$> o .: "competitionId"
      end <- o .: "end"
      formuleEventID <- readIntAsText <$> o .: "eventId"
      index <- readIntAsText <$> o .: "index"
      formuleLabel <- o .: "label"
      marketID <- readIntAsText <$> o .: "marketId"
      marketType <- o .: "marketType"
      marketTypeGroup <- o .: "marketTypeGroup"
      marketTypeID <-
        (readIntAsText <$> o .: "marketTypeId") <|> o .: "marketTypeId"
      outcomes <- o .: "outcomes"
      sportID <- o .: "sportId"
      return Formule {..}

{- Outcome -}
data Outcome = Outcome
  { cote :: Double
  , outcomeLabel :: OutcomeType
  , pos :: Int
  , status :: Int
  , trend :: Trend
  , winner :: Winner
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

data Sport
  = Football
  | Tennis
  | BasketBall
  | Rugby
  | VolleyBall
  | Formule1
  | Baseball
  | BeachVolley
  | Athletism
  | Swimming
  deriving (Eq, Show)

instance AE.FromJSON Sport where
  parseJSON =
    AE.withText "sport" $ \s ->
      case s of
        "100" -> return Football
        "600" -> return Tennis
        "601600" -> return BasketBall
        "964500" -> return Rugby
        "1200" -> return VolleyBall
        "1300" -> return Formule1
        "433100" -> return Baseball
        "1250" -> return BeachVolley
        "964700" -> return Athletism
        "2400" -> return Swimming
        _ -> fail $ "Unknown sport with id" ++ show s

data MarketType
  = HalfTime
  | Handicap0_1
  | Handicap1_0
  | ExactScore
  | HTFT
  | DoubleChance
  | PM1_5
  | PM2_5
  | PM3_5
  | PM4_5
  | FullTime
  | FirstToScore
  | ResultPlusMinus
  deriving (Eq, Show)

instance AE.FromJSON MarketType where
  parseJSON =
    AE.withText "market type" $ \s ->
      case s of
        "Mi-Temps" -> return HalfTime
        "Handicap [0:1]" -> return Handicap0_1
        "Handicap [1:0]" -> return Handicap1_0
        "Score exact" -> return ExactScore
        "MT/FM" -> return HTFT
        "Double chance" -> return DoubleChance
        "Plus/Moins 1,5 but (Temps réglementaire)" -> return PM1_5
        "Plus/Moins 2,5 buts (Temps réglementaire)" -> return PM2_5
        "Plus/Moins 3,5 buts (Temps réglementaire)" -> return PM3_5
        "1/N/2" -> return FullTime
        "1er but" -> return FirstToScore
        "Résultat & Plus/moins" -> return ResultPlusMinus
        _ -> fail $ "Unknown MarketType " ++ show s

data OutcomeType
  = OutcomeWinner OutcomeWinner
  | OutcomeExactScore OutcomeExactScore
  | OutcomeHTFT OutcomeHTFT
  | OutcomeMoreLess OutcomeMoreLess
  deriving (Eq, Show)

instance AE.FromJSON OutcomeType where
  parseJSON o =
    (OutcomeWinner <$> AE.parseJSON o) <|>
    (OutcomeExactScore <$> AE.parseJSON o) <|>
    (OutcomeHTFT <$> AE.parseJSON o) <|>
    (OutcomeMoreLess <$> AE.parseJSON o)

data OutcomeWinner
  = Team1
  | Draw
  | Team2
  deriving (Eq, Show)

instance AE.FromJSON OutcomeWinner where
  parseJSON =
    AE.withText "winner 1/N/2" $ \s ->
      case s of
        "1" -> return Team1
        "N" -> return Draw
        "2" -> return Team2
        _ -> fail $ "Unknown 1/N/2 outcome" ++ show s

data OutcomeExactScore
  = Score Int
          Int
  | AllOthers
  deriving (Eq, Show)

instance AE.FromJSON OutcomeExactScore where
  parseJSON =
    AE.withText "score x - y" $ \s ->
      case TE.splitOn " - " s of
        [a, b] -> return $ Score (readText a) (readText b)
        ["Autres"] -> return AllOthers
        _ -> fail $ "Can't parse score " ++ show s
    where
      readText :: (Read a) => TE.Text -> a
      readText = read . TE.unpack

data OutcomeHTFT =
  MkOutcomeHTFT OutcomeWinner
                OutcomeWinner
  deriving (Eq, Show)

instance AE.FromJSON OutcomeHTFT where
  parseJSON =
    AE.withText "winner half-time full-time" $ \s ->
      case TE.splitOn "/" s of
        [a, b] -> MkOutcomeHTFT <$> toParser a <*> toParser b
        _ -> fail $ "Unknown winner half-time full-time" ++ show s
    where
      toParser :: TE.Text -> AT.Parser OutcomeWinner
      toParser = AT.parseJSON . AT.String

data OutcomeMoreLess
  = More
  | Less
  deriving (Eq, Show)

instance AE.FromJSON OutcomeMoreLess where
  parseJSON =
    AE.withText "outcome more/less" $ \s ->
      case s of
        "Plus" -> return More
        "Moins" -> return Less
        _ -> fail $ "Unknown more/less outcome" ++ show s

data MatchLineUp =
  MkMatchLineUp String
                String
  deriving (Eq, Show)

instance AE.FromJSON MatchLineUp where
  parseJSON =
    AE.withText "match line-up" $ \s ->
      case TE.splitOn "-" s of
        [a, b] -> return $ MkMatchLineUp (TE.unpack a) (TE.unpack b)
        _ -> fail $ "Can't parse match line-up" ++ show s
