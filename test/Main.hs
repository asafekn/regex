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
        evaluate (Or (and (MatchChar 'a') (MatchChar 'b')) (and (MatchChar 'x') (MatchChar 'y'))) "abc" `shouldBe` Just "ab"

      it "Or matches second alternative with two characters" $
        evaluate (Or (and (MatchChar 'a') (MatchChar 'b')) (and (MatchChar 'x') (MatchChar 'y'))) "xyc" `shouldBe` Just "xy"


-- Or matches first alternative with two characters
--
-- run isStart regex str
-- run True (Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) "abc"
-- 	Or x y ->
-- 	Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) ->
-- 		case run isStart x str of
-- 		case run True (and (MatchChar 'a') (MatchChar 'b')) "abc" of
-- 			And x y -> do
-- 			And (MatchChar 'a') (MatchChar 'b') -> do
-- 		        	(r1, str') <- run isStart x str
-- 		        	(r1, str') <- run True (MatchChar 'a') "abc"
-- 		        		MatchChar c ->
-- 		        		MatchChar 'a' ->
-- 		        			case str of
-- 		        			case "abc" of
-- 		        				x : xs ->
-- 		        				"a" : "bc" ->
-- 		        					if x == c
-- 		        					if "a" == 'a'
-- 		        					then [([x], xs)]
-- 		        					then [(["a"], "bc")]
-- 		        	(["a"], "bc") <- run True (MatchChar 'a') "abc"
-- 		        	let isStart' = isStart && r1 == ""
-- 		        	let isStart' = True && ["a"] == ""
-- 		        	(r2, str'') <- run isStart' y str'
-- 		        	(r2, str'') <- run False (MatchChar 'b') "bc"
-- 		        		MatchChar c ->
-- 		        		MatchChar 'b' ->
-- 		        			case str of
-- 		        			case "bc" of
-- 		        				x : xs ->
-- 		        				"b" : "c" ->
-- 		        					if x == c
-- 		       						if "b" == 'b'
-- 		       						then [([x], xs)]
-- 		       						then [(["b"], "c")]
-- 		        	(["b"], "c") <- run False (MatchChar 'b') "bc"
-- 		        	return (r1 <> r2, str'')
-- 		        	return (["a"] <> ["b"], "c")
-- 		        	return (["ab"], "c")
-- 		case (["ab"], "c") of
-- 		r -> r
--		(["ab"], "c") -> (["ab"], "c")
--
--
--
-- Or matches second alternative with two characters
--
-- run isStart regex str
-- run True (Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) "xyc"
-- 	Or x y ->
-- 	Or (And (MatchChar 'a') (MatchChar 'b')) (And (MatchChar 'x') (MatchChar 'y'))) ->
-- 		case run isStart x str of
-- 		case run True (and (MatchChar 'a') (MatchChar 'b')) "xyc" of
-- 			And x y -> do
-- 			And (MatchChar 'a') (MatchChar 'b') -> do
-- 		        	(r1, str') <- run isStart x str
-- 		        	(r1, str') <- run True (MatchChar 'a') "xyc"
-- 		        		MatchChar c ->
-- 		        		MatchChar 'a' ->
-- 		        			case str of
-- 		        			case "xyc" of
-- 		        				x : xs ->
-- 		        				"x" : "yc" ->
-- 		        					if x == c
-- 		        					if "x" == 'a'
-- 		        					else []
-- 		        	[] <- run True (MatchChar 'a') "abc"
-- 		case [] of
--		[] -> run isStart y str
--		[] -> run True (And (MatchChar 'x') (MatchChar 'y')) "xyc"
--			And x y -> do
-- 			And (MatchChar 'x') (MatchChar 'y') -> do
-- 		        	(r1, str') <- run isStart x str
-- 		        	(r1, str') <- run True (MatchChar 'x') "xyc"
-- 		        		MatchChar c ->
-- 		        		MatchChar 'x' ->
-- 		        			case str of
-- 		        			case "xyc" of
-- 		        				x : xs ->
-- 		        				"x" : "yc" ->
-- 		        					if x == c
-- 		        					if "x" == 'x'
-- 		        					then [([x], xs)]
-- 		        					then [(["x"], "yc")]
-- 		        	(["x"], "yc") <- run True (MatchChar 'x') "xyc"
-- 		        	let isStart' = isStart && r1 == ""
-- 		        	let isStart' = True && ["x"] == ""
-- 		        	(r2, str'') <- run isStart' y str'
-- 		        	(r2, str'') <- run False (MatchChar 'y') "yc"
-- 		        		MatchChar c ->
-- 		        		MatchChar 'y' ->
-- 		        			case str of
-- 		        			case "yc" of
-- 		        				x : xs ->
-- 		        				"y" : "c" ->
-- 		        					if x == c
-- 		       						if "y" == 'y'
-- 		       						then [([x], xs)]
-- 		       						then [(["y"], "c")]
-- 		        	(["y"], "c") <- run False (MatchChar 'y') "yc"
-- 		        	return (r1 <> r2, str'')
-- 		        	return (["x"] <> ["y"], "c")
-- 		        	return (["xy"], "c")
--
--


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


