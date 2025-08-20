module Main (main) where

import Test.Hspec (hspec, describe, shouldBe, it)
import Regex

main :: IO ()
main = hspec $ do
  describe "regex" $ do
    describe "evaluate" $ do
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

      it "matches any character" $
        evaluate MatchAny "x" `shouldBe` Just "x"

      it "matches any character between other characters" $
        evaluate (And (MatchChar 'a') (And MatchAny (MatchChar 'b'))) "axb" `shouldBe` Just "axb"

      it "Or matches first alternative" $
        evaluate (Or (MatchChar 'a') (MatchChar 'b')) "ac" `shouldBe` Just "a"

      it "Or matches second alternative" $
        evaluate (Or (MatchChar 'a') (MatchChar 'b')) "bc" `shouldBe` Just "b"





    describe "compile" $ do
      it "MatchStart" $
        compile "^" `shouldBe` MatchStart

      it "MatchEnd" $
        compile "$" `shouldBe` MatchEnd

      it "MatchChar" $
        compile "a" `shouldBe` MatchChar 'a'

      it "MatchAny" $
        compile "." `shouldBe` MatchAny

      it "multiple MatchChar" $
        compile "aa" `shouldBe` And (MatchChar 'a') (MatchChar 'a')

      it "Asterisk" $
        compile "a*" `shouldBe` Asterisk (MatchChar 'a')

      it "Plus" $
        compile "a+" `shouldBe` Plus (MatchChar 'a')

      it "Quesstion mark" $
        compile "a?" `shouldBe` Question (MatchChar 'a')

      it "everything together" $
        compile "^aa+b*$" `shouldBe`
          (And (And (And (And MatchStart (MatchChar 'a')) (Plus (MatchChar 'a'))) (Asterisk (MatchChar 'b'))) MatchEnd)

    describe "run" $ do
      it "matches one character at the beginning" $
        evaluate (compile "a") "a" `shouldBe` Just "a"

      it "matches one character anywhere in the string" $
        evaluate (compile "a") "bac" `shouldBe` Just "a"

      it "Plus matches once" $
        evaluate (compile "a+") "a" `shouldBe` Just "a"

      it "Plus matches more than once" $
        evaluate (compile "a+") "aaa" `shouldBe` Just "aaa"

      it "Plus does not match zero times" $
        evaluate (compile "a+") "b" `shouldBe` Nothing

      it "Matches from the start" $
        evaluate (compile "^a") "ab" `shouldBe` Just "a"

      it "MatchStart does not match after the start" $
        evaluate (compile "^a") "ba" `shouldBe` Nothing

      it "matches any character" $
        evaluate (compile ".") "x" `shouldBe` Just "x"

      it "matches any character between other characters" $
        evaluate (compile "a.b") "axb" `shouldBe` Just "axb"

      it "Asterisk matches once" $
        evaluate (compile "a*") "a" `shouldBe` Just "a"

      it "Asterisk matches more than once" $
        evaluate (compile "a*") "aaa" `shouldBe` Just "aaa"

      it "Asterisk matches zero times" $
        evaluate (compile "a*") "b" `shouldBe` Just ""

      it "$ matches the end" $
        evaluate (compile "a$") "aa" `shouldBe` Just "a"

      it "$ does not match if it is not the end" $
        evaluate (compile "a$") "aab" `shouldBe` Nothing

      it "Question mark matches once" $
        evaluate (compile "a?") "a" `shouldBe` Just "a"

      it "Question mark does not match more than once" $
        evaluate (compile "a?") "aaa" `shouldBe` Just "a"

      it "Question mark matches zero times" $
        evaluate (compile "a?") "b" `shouldBe` Just ""

      it "Test brackets" $
        evaluate (compile "c(ab)+") "ccabab" `shouldBe` Just "cabab"

      it "Test escaping" $
        evaluate (compile "a\\?b") "ca?bc" `shouldBe` Just "a?b"









--     compile "$" == MatchEnd
--     compile "a+" == Plus (MatchChar 'a')
--     compile "a*" == Asterisk (MatchChar 'a')
--     compile "aba*d+" == ....


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


