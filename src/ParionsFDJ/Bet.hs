module ParionsFDJ.Bet
  ( toBetSum
  ) where

import qualified Control.Monad as MO
import qualified HBet.Bet as HB
import qualified HBet.BetSum as HBS
import qualified HBet.Football as FB
import ParionsFDJ.JSON

toBetSum :: [Event] -> [HBS.BetSum]
toBetSum events = do
  event <- events
  case eventSportID event of
    Football -> footballToBetSum event
    _ -> []

footballToBetSum :: Event -> [HBS.BetSum]
footballToBetSum event = do
  match <- eventToFootBallMatch event
  formule <- formules event
  case marketType formule of
    HalfTime ->
      return $
      HBS.FootballHalfTime $
      FB.FootballBetInfo match (formuleToHbet strToHalfTime formule)
    _ -> []

eventToFootBallMatch :: Event -> [FB.FootballMatch]
eventToFootBallMatch event =
  case formules event of
    form:_ ->
      let (MkMatchLineUp team1 team2) = formuleLabel form
          competition = undefined
      in return $ FB.FootballMatch team1 team2 competition
    [] -> []

formuleToHbet :: (OutcomeType -> [a]) -> Formule -> [HB.Choice a]
formuleToHbet fa formule = do
  outcome <- outcomes formule
  betType <- fa $ outcomeLabel outcome
  let odd = cote outcome
  return $ HB.Choice betType odd

strToHalfTime :: (MO.MonadPlus m) => OutcomeType -> m FB.FootballHalfTime
strToHalfTime (OutcomeWinner winner) =
  case winner of
    Team1 -> return FB.HT1
    Draw -> return FB.HTDraw
    Team2 -> return FB.HT2
