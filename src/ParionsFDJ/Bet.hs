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
  valider <- getValider formule outcome
  return $ HB.Choice () valider match (cote outcome)
  where
    getValider formule outcome =
      case (marketType formule, outcomeLabel outcome) of
        (HalfTime, OutcomeWinner bet) -> return $ halfTimeToPredicate bet
        (FullTime, OutcomeWinner bet) -> return $ fullTimeToPredicate bet
        (ExactScore, OutcomeExactScore bet) ->
          return $ exactScoreToPredicate bet
        (HTFT, OutcomeHTFT bet) -> return $ htftToPredicate bet
        _ -> []

{- Helpers -}
fullTimeToPredicate ::
     OutcomeWinner -> HB.Score FB.FootballMatch -> HB.BetResult
fullTimeToPredicate t (FB.FootballScore h1 h2 f1 f2)
  | op t (h1 + f1) (h2 + f2) = HB.Win
  | otherwise = HB.Lose
  where
    op Team1 = (>)
    op Draw = (==)
    op Team2 = (<)

halfTimeToPredicate ::
     OutcomeWinner -> HB.Score FB.FootballMatch -> HB.BetResult
halfTimeToPredicate t (FB.FootballScore h1 h2 _ _)
  | op t h1 h2 = HB.Win
  | otherwise = HB.Lose
  where
    op Team1 = (>)
    op Draw = (==)
    op Team2 = (<)

exactScoreToPredicate ::
     OutcomeExactScore -> HB.Score FB.FootballMatch -> HB.BetResult
exactScoreToPredicate (Score s1 s2) = HB.winOrLose f
  where
    f (FB.FootballScore h1 h2 f1 f2) = (h1 + f1 == s1) && (h2 + f2 == s2)

htftToPredicate :: OutcomeHTFT -> HB.Score FB.FootballMatch -> HB.BetResult
htftToPredicate (MkOutcomeHTFT win1 win2) score =
  case (halfTimeToPredicate win1 score, fullTimeToPredicate win2 score) of
    (HB.Win, HB.Win) -> HB.Win
    _ -> HB.Lose
