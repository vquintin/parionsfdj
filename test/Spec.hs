{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import ParionsFDJ.JSON
import Test.HUnit

main :: IO ()
main = showCounts <$> runTestTT tests >>= putStrLn
  where
    tests = TestList [testParseTrend]

testParseTrend = parseTestWithCases "Error in parsing trend" cases
  where
    cases =
      [ ("\"-1\"", Just Down)
      , ("\"0\"", Just Nil)
      , ("\"1\"", Just Up)
      , ("x", Nothing)
      ]

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
