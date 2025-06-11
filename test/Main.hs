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

    it "Matches from the start" $
      evaluate (And MatchStart (MatchChar 'a')) "ab" `shouldBe` Just "a"

    it "MatchStart does not match after the start" $
      evaluate (And MatchStart (MatchChar 'a')) "ba" `shouldBe` Nothing


-- evaluate (Plus (MatchChar 'a')) "aaa"
-- asum $ fmap (parse (regexParser (Plus (MatchChar 'a'))) $ tails "aaa"
-- asum $ fmap (parse (regexParser (Plus (MatchChar 'a'))) $ ["aaa", "aa", "a"]
-- parse (regexParser (Plus (MatchChar 'a')) "aaa"
-- run (Plus (MatchChar 'a')) "aaa"
--  (r, str') <- run (MatchChar 'a') "aaa"
--    run (MatchChar 'a') "aaa"
--  (r, str') <- [(['a'], "aa")]
--  (['a'], "aa")
--  (rs, str'') <- zeroOrMore (MatchChar 'a') "aa"
--    zeroOrMore (MatchChar 'a') "aa"
--      run (MatchChar 'a') "aa"
--      [(['a'], ['a'])]
--      (x, str') <- [(['a'], ['a'])]
--      (y, str'') <- zeroOrMore (MatchChar 'a') ['a']
--        zeroOrMore (MatchChar 'a') ['a']
--          run (MatchChar 'a') ['a']
--          [(['a'],[])]
--          (x, str') <- [(['a'],[])]
--          (y, str'') <- zeroOrMore (MatchChar 'a') []
--            run (MatchChar 'a') []
--            []
--            [] -> [("", [])]
--          (y, str'') <- [("", [])]
--          ("", []) <- [("", [])]
--          [("", ['a']), (['a'], [])]
--      (y, str'') <- [("", ['a']), (['a'], [])]
--        (y, str'') == ("", ['a'])
--          [("", "aa"), (['a'], ['a'])]
--        (y, str'') == (['a'], [])
--          [("", "aa"), ( ['a'] <> ['a'], [])]
--          [("", "aa"), ( ["aa"], [])]
--      [("", "aa"), (['a'], ['a']), ("", "aa"), ( ["aa"], [])]
--  (rs, str'') <- [("", "aa"), (['a'], ['a']), ("", "aa"), ( ["aa"], [])]
--    (rs, str'') <- ("", "aa")
--      return (['a'] <> [], "aa")
--      [(['a'] <> [], "aa")]
--      [(['a'], "aa")]
--    (rs, str'') <- (['a'], ['a'])
--      return (['a'] <> ['a'], ['a'])
--      return ("aa", ['a'])
--
--    (rs, str'') <- ("", "aa")
--      return (['a'], "aa")
--
--    (rs, str'') <- ( "aa", [])]
--      return (['a'] <> "aa", [])
--      return ("aaa", [])
--  [(['a'], "aa"), ("aa", ['a']), (['a'], "aa"), ("aaa", [])]
--
--
--
--
--

