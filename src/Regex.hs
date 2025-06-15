module Regex where

import Control.Applicative (asum)
import Data.List (tails)

data Parser a = ParserConstructor (String -> [(a, String)])

parse :: Parser a -> String -> Maybe a
parse (ParserConstructor f) str =
  case f str of
    [] -> Nothing
    (x, _) : _ -> Just x

data Regex
  = MatchStart      -- '^'
  | MatchEnd        -- '$'
  | MatchChar Char  -- 'a'
  | And Regex Regex -- "aa"
  | Plus Regex      -- '+'
  | Asterisk Regex  -- '*'

type Match = String

evaluate :: Regex -> String -> Maybe Match
evaluate rgx s =
    parse (regexParser rgx) s

-- No parenthesis for now.
--
-- compile "^a$" == And MatchStart (And (MatchChar 'a') MatchEnd)
-- compile "^" == MatchStart
-- compile "$" == MatchEnd
-- compile "a+" == Plus (MatchChar 'a')
-- compile "a*" == Asterisk (MatchChar 'a')
-- compile "aba*d+" == ....
compile :: String -> Regex
compile = undefined

regexParser :: Regex -> Parser String
regexParser r0 = ParserConstructor f
  where
  f :: String -> [(String, String)]
  f str =
    case tails str of
      x : xs -> (run True r0 x) <> (concat $ fmap (run False r0) xs)
      [] -> error "Impossible"


  run :: Bool -> Regex -> String -> [(String, String)]
  run isStart regex str =
    case regex of
      MatchStart ->
        if isStart
        then [("" , str)]
        else []

      MatchEnd ->
        case str of
          [] -> [("" , "")]
          _ :_ -> []

      MatchChar c ->
        case str of
          [] -> []
          x : xs ->
            if x == c
            then [ ([x], xs) ]
            else []

      And x y -> do
        (r1, str') <- run isStart x str
        let isStart' = isStart && r1 == ""
        (r2, str'') <- run isStart' y str'
        return (r1 <> r2, str'')

      Plus x -> do
        (r, str') <- run isStart x str
        let isStart' = isStart && r == ""
        (rs, str'') <- zeroOrMore isStart' x str'
        return (r <> rs, str'')

      Asterisk x ->
        zeroOrMore isStart x str

  zeroOrMore :: Bool -> Regex -> String -> [(String, String)]
  zeroOrMore isStart r str = more <> zero
    where
    zero = [("", str)]
    more = do
      (x, str') <- run isStart r str
      let isStart' = isStart && x == ""
      (y, str'') <- zeroOrMore isStart' r str'
      return (x <> y, str'')
