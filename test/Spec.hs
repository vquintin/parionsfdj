{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BS
import Data.Time
import ParionsFDJ.JSON
import Test.HUnit

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where
    tests =
      TestList
        [testParseTrend, testParseOutcome, testParseFormule, testParseBetJSON]

testParseBetJSON :: Test
testParseBetJSON =
  TestCase $ do
    input <- BS.readFile "test/ex.json"
    let events = parseBetJSON input
    assertBool "event list is empty" $ not (null events)

testParseEvents :: Test
testParseEvents =
  TestCase $ do
    input <- BS.readFile "test/ex.json"
    let parsed = AE.eitherDecode input :: Either String [Event]
    case parsed of
      Left err -> assertFailure err
      Right _ -> return ()

testParseFormule :: Test
testParseFormule =
  TestCase $ do
    input <- BS.readFile "test/formule.json"
    assertParsingEqual "Error in parsing formule" input expected
  where
    expected =
      Just
        Formule
        { competition = FootballCompetition $ ChD1 Romania
        , competitionID = 1384
        , end =
            parseTimeOrError
              False
              defaultTimeLocale
              "%FT%X%z"
              "2017-07-28T19:55:00+02:00"
        , formuleEventID = 384006
        , index = 529
        , formuleLabel = MkMatchLineUp "Con.Chiajna" "StudentescIasi"
        , marketID = 1939855
        , marketType = HalfTime
        , marketTypeGroup = "Mi-Temps"
        , marketTypeID = 2
        , outcomes =
            [ Outcome 2.35 (OutcomeWinner Team1) 1 2 Down (Bool False)
            , Outcome 1.75 (OutcomeWinner Draw) 2 2 Nil (Bool False)
            , Outcome 3.90 (OutcomeWinner Team2) 3 2 Up (Bool False)
            ]
        , sportID = Football
        }

testParseOutcome :: Test
testParseOutcome =
  TestCase $ do
    input <- BS.readFile "test/outcome.json"
    assertParsingEqual "Error in parsing outcome" input expected
  where
    expected = Just $ Outcome 2.35 (OutcomeWinner Team1) 1 2 Down (Bool False)

testParseTrend :: Test
testParseTrend = parseTestWithCases "Error in parsing trend" cases
  where
    cases =
      [ ("\"-1\"", Just Down)
      , ("\"0\"", Just Nil)
      , ("\"1\"", Just Up)
      , ("x", Nothing)
      ]

{- helpers -}
parseTest ::
     (Eq a, AE.FromJSON a, Show a) => String -> BS.ByteString -> Maybe a -> Test
parseTest preface input expected =
  TestCase $ assertParsingEqual preface input expected

parseTestWithCases ::
     (Eq a, AE.FromJSON a, Show a)
  => String
  -> [(BS.ByteString, Maybe a)]
  -> Test
parseTestWithCases preface cases = TestCase $ foldl f (return ()) cases
  where
    f assertion (input, expected) =
      assertion >> assertParsingEqual preface input expected

assertParsingEqual ::
     (Eq a, AE.FromJSON a, Show a)
  => String
  -> BS.ByteString
  -> Maybe a
  -> Assertion
assertParsingEqual preface input expected =
  assertEqual preface expected $ AE.decode input
