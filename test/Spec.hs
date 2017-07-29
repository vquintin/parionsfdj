{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import ParionsFDJ.JSON
import Test.HUnit

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where
    tests = TestList [testParseTrend, testParseOutcome]

testParseOutcome :: Test
testParseOutcome =
  TestCase $ do
    input <- BS.readFile "test/outcome.json"
    assertParsingEqual "Error in parsing outcome" input expected
  where
    expected = Just $ Outcome 2.35 "1" 1 2 Down False

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
     (Eq a, FromJSON a, Show a) => String -> BS.ByteString -> Maybe a -> Test
parseTest preface input expected =
  TestCase $ assertParsingEqual preface input expected

parseTestWithCases ::
     (Eq a, FromJSON a, Show a) => String -> [(BS.ByteString, Maybe a)] -> Test
parseTestWithCases preface cases = TestCase $ foldl f (return ()) cases
  where
    f assertion (input, expected) =
      assertion >> assertParsingEqual preface input expected

assertParsingEqual ::
     (Eq a, FromJSON a, Show a)
  => String
  -> BS.ByteString
  -> Maybe a
  -> Assertion
assertParsingEqual preface input expected =
  assertEqual preface expected $ decode input
