module ParionsFDJ.Bet
  (
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
  let match = eventToFootBallMatch event
  formule <- formules event
  case marketType formule of
    HalfTime ->
      return $
      HBS.FootballHalfTime $
      FB.FootballBetInfo match (formuleToHbet strToHalfTime formule)
    _ -> []

eventToFootBallMatch :: Event -> FB.FootballMatch
eventToFootBallMatch = undefined

formuleToHbet :: (String -> [a]) -> Formule -> [HB.Choice a]
formuleToHbet fa formule = do
  outcome <- outcomes formule
  betType <- fa $ outcomeLabel outcome
  let odd = cote outcome
  return $ HB.Choice betType odd

strToHalfTime :: (MO.MonadPlus m) => String -> m FB.FootballHalfTime
strToHalfTime s =
  case s of
    "1" -> return FB.HT1
    "N" -> return FB.HTDraw
    "2" -> return FB.HT2
    _ -> MO.mzero
