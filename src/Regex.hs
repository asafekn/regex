module Regex where

import Data.List (tails)
import Data.Char (isDigit)

test :: String -> String -> Bool
test regex str =
  case regex of
    '^' : rs -> testAux rs str
    _ -> or (map (testAux regex) (tails str))

testAux :: String -> String -> Bool
testAux regex str =
  case regex of
    [] -> True
    '$' : _ -> str == []

    c : '*' : rs ->
      case str of
        [] -> testAux rs str
        s : ss ->
          if s == c
          then testAux regex ss || testAux rs str || testAux rs ss
          else testAux rs str

    c : '+' : rs ->
      case str of
         [] -> False
         s : ss ->
          if s == c
          then testAux regex ss || testAux rs ss
          else False

    c : rs ->
      case str of
        [] -> False
        s : ss ->
          if c == s
          then testAux rs ss
          else False


data Parser a = ParserConstructor (String -> [(a, String)])

parserInt :: Parser Int
parserInt = ParserConstructor f
  where
  f :: String -> [(Int, String)]
  f str =
    case span isDigit str of
      ([], _) -> []
      (digits, rest) -> [(read digits, rest)]

parse :: Parser a -> String -> Maybe a
parse (ParserConstructor f) str =
  case f str of
    [] -> Nothing
    [(x, _)] -> Just x
    _ : _ -> error "ambiguous result"

data Regex
  = MatchEnd        -- '$'
  | MatchChar Char  -- 'a'
  | And Regex Regex -- "aa"
  | Plus Regex      -- '+'
  | Asterisk Regex  -- '*'

type Match = String

evaluate :: Regex -> String -> Maybe Match
evaluate regex str = parse (regexParser regex) str

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
  zeroOrMore r str =
    case run r str of
      [] -> [("", str)]
      xs -> do
        (x, str') <- xs
        (y, str'') <- zeroOrMore r str'
        [("", str), (x <> y, str'')]

