module Regex where

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
  | AnyChar         -- '.'
  deriving (Show, Eq)

type Match = String

evaluate :: Regex -> String -> Maybe Match
evaluate rgx s =
    parse (regexParser rgx) s

compile :: String -> Regex
compile str =
  case str of
    [] -> error "empty"

    '\' : c : rest -> concatRegex (specialCharCompile c) rest

    c : '*' : rest -> concatRegex (Asterisk $ replace c) rest

    c : '+' : rest -> concatRegex (Plus (replace c)) rest

    c : rest -> concatRegex (replace c) rest

specialCharCompile :: Char -> Regex
specialCharCompile c =
  case c of
    '*' -> MatchChar c
    '+' -> MatchChar c
    '^' -> MatchChar c
    '$' -> MatchChar c
    '.' -> MatchChar c
    '\' -> MatchChar c
    _ -> error "Syntax error"

concatRegex :: Regex -> String -> Regex
concatRegex reg str =
  case str of
    [] -> reg
    _ -> And reg (compile str)

replace :: Char -> Regex
replace c =
  case c of
    '^' -> MatchStart
    '$' -> MatchEnd
    '.' -> AnyChar
    '\' -> error "Syntaxe error"
    _   -> MatchChar c


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

      AnyChar ->
      	case str of
	  [] -> []
	  x : xs -> [([x], xs)]

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
