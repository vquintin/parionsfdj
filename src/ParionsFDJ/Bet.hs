module ParionsFDJ.Bet
  ( toBetSum
  ) where

import qualified Control.Monad as MO
import qualified HBet.Bet as HB
import qualified HBet.BetSum as HBS
import qualified HBet.Football as FB
import ParionsFDJ.JSON

toBetSum :: [Event] -> [HBS.ChoiceSum ()]
toBetSum events = do
  event <- events
  case eventSportID event of
    Football -> undefined
    _ -> []

type FDJChoice a = HB.Choice a ()

extractChoices :: (Formule -> [FDJChoice a]) -> Event -> [FDJChoice a]
extractChoices ex event = formules event >>= ex

{- Football -}
extractFootballMatch :: Formule -> [FB.FootballMatch]
extractFootballMatch formule = do
  let (FootballCompetition comp) = competition formule
  let (MkMatchLineUp team1 team2) = formuleLabel formule
  event <- getEvent comp
  return $ FB.FootballMatch team1 team2 event
  where
    getEvent (ChD1 France) = return FB.Ligue1
    getEvent _ = []

extractFootballChoices :: Formule -> [FDJChoice FB.FootballMatch]
extractFootballChoices formule = do
  match <- extractFootballMatch formule
  outcome <- outcomes formule
  let valider = getValider formule outcome
  return $ HB.Choice () valider match (cote outcome)
  where
    getValider formule outcome = undefined
