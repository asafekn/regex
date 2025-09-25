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

      it "Or matches first alternative with two characters" $
        evaluate (Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) "abc" `shouldBe` Just "ab"

      it "Or matches second alternative with two characters" $
        evaluate (Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) "xyc" `shouldBe` Just "xy"

      it "Negation does not match any of given chars" $
        evaluate (Negation ['a','b']) "abc" `shouldBe` Just "c"

      it "Negation does not match on empty string" $
        evaluate (Negation ['a','b']) "" `shouldBe` Nothing

      it "fixed quantification matches" $
        evaluate (Quantified (2,3) (MatchChar 'a')) "aaaa" `shouldBe` Just "aaa"

      it "fixed quantification matches with equal numbers" $
        evaluate (Quantified (2,2) (MatchChar 'a')) "aaaa" `shouldBe` Just "aa"

      it "fixed quantification matches starting with 0" $
        evaluate (Quantified (0,2) (MatchChar 'a')) "aaaa" `shouldBe` Just "aa"

      it "fixed quantification matches starting with 0" $
        evaluate (Quantified (0,2) (MatchChar 'a')) "bbbb" `shouldBe` Just ""

      it "fixed quantification matches starting with second argument bigger than strig" $
        evaluate (Quantified (1,9) (MatchChar 'a')) "aaaa" `shouldBe` Just "aaaa"

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

      it "Or" $
        compile "a|b" `shouldBe` Or (MatchChar 'a') (MatchChar 'b')

      it "Or with more alternatives" $
        compile "a|b|c" `shouldBe` Or (Or (MatchChar 'a') (MatchChar 'b')) (MatchChar 'c')

      it "Or with Ands" $
        compile "ab|cd" `shouldBe` Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'c') (MatchChar 'd'))

      it "Or with parentesis" $
        compile "(a|b)c" `shouldBe` And (Group (Or (MatchChar 'a') (MatchChar 'b'))) (MatchChar 'c')

      it "[] works" $
        compile "[abc]" `shouldBe` Or (Or (MatchChar 'a') (MatchChar 'b')) (MatchChar 'c')

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

      it "Or" $
        evaluate (compile "a|b") "b" `shouldBe` Just "b"

      it "Or with more alternatives" $
        evaluate (compile "a|b|c") "abc" `shouldBe` Just "a"

      it "Or with Ands" $
        evaluate (compile "ab|cd") "cd" `shouldBe` Just "cd"

      it "Or with parentesis" $
        evaluate (compile "(a|b)c") "abcd" `shouldBe` Just "bc"

      it "[] works" $
        evaluate (compile "[abc^$+?*|]+") "Kabc^$+?*|K" `shouldBe` Just "abc^$+?*|"

