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

    it "Plus matches once" $
      evaluate (Plus (MatchChar 'a')) "a" `shouldBe` Just "a"

    it "Plus matches more than once" $
      evaluate (Plus (MatchChar 'a')) "aaa" `shouldBe` Just "aaa"

    it "Plus does not match zero times" $
      evaluate (Plus (MatchChar 'a')) "b" `shouldBe` Nothing
