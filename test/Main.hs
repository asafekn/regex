module Main (main) where

import Test.Hspec (hspec, describe, shouldBe, it)
import Regex

main :: IO ()
main = hspec $ do
  describe "regex" $ do
    it "matches one character at the beginning" $
      evaluate (MatchChar 'a') "a" `shouldBe` Just "a"

    it "matches one character anywhere in the string" $
      evaluate (MatchChar 'a') "bac" `shouldBe` Just "a"



