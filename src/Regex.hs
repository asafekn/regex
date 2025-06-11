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
  = MatchEnd        -- '$'
  | MatchChar Char  -- 'a'
  | And Regex Regex -- "aa"
  | Plus Regex      -- '+'
  | Asterisk Regex  -- '*'
  | MatchStart      -- '^'

type Match = String

evaluate :: Regex -> String -> Maybe Match
evaluate regex str =
  asum $ fmap (parse (regexParser regex)) $ tails str

regexParser :: Regex -> Parser String
regexParser r0 = ParserConstructor (run r0)
  where
  run :: Regex -> String -> [(String, String)]
  run regex str =
    case regex of
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
        (r1, str') <- run x str
        (r2, str'') <- run y str'
        return (r1 <> r2, str'')

      Plus x -> do
        (r, str') <- run x str
        (rs, str'') <- zeroOrMore x str'
        return (r <> rs, str'')

      Asterisk x ->
        zeroOrMore x str

  zeroOrMore :: Regex -> String -> [(String, String)]
  zeroOrMore r str = more <> zero
    where
    zero = [("", str)]
    more = do
      (x, str') <- run r str
      (y, str'') <- zeroOrMore r str'
      return (x <> y, str'')
