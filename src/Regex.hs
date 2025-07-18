{-# LANGUAGE InstanceSigs #-}
module Regex where

import Data.List (tails)
import Prelude hiding (lex)
import Control.Applicative (Alternative, (<|>), empty, many)

data SParser a = ParserConstructor (String -> [(a, String)])

parseS :: SParser a -> String -> Maybe a
parseS (ParserConstructor f) str =
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
  | Question Regex  -- '?'
  | MatchAny        -- '.'
  deriving (Show, Eq)

type Match = String

evaluate :: Regex -> String -> Maybe Match
evaluate rgx s =
    parseS (matchParser rgx) s

compile :: String -> Regex
compile str =
  case str of
    [] -> error "empty"

    '\\' : c : rest -> concatRegex (specialCharCompile c) rest

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
    '\\' -> MatchChar c
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
    '.' -> MatchAny
    '\\' -> error "Syntaxe error"
    _   -> MatchChar c

-- ============================================
-- Matching
-- ============================================

matchParser :: Regex -> SParser String
matchParser r0 = ParserConstructor f
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

      MatchAny ->
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

-- ============================================
-- New approach
-- ============================================
--  Compilador
--
--  read              File -> Text
--  lex               Text -> Tokens
--  parse             Tokens -> Abstract Syntax Tree (AST)
--  type-checking     AST -> AST (enriched)
--  optimisations     AST -> AST
--  compilation       AST -> Low level code
--  code generation   Low level code -> Machine code

-- ============================================
-- Lexing
-- ============================================

data Token
  = Token Char
  | TokenPlus
  | TokenAsterisk
  | TokenQuestionMark
  | TokenDollar
  | TokenCaret
  | TokenDot
  | TokenParenthesisOpen
  | TokenParenthesisClose
  deriving (Show, Eq)

lex :: String -> [Token]
lex xs =
  case xs of
    [] -> []
    '*' : rest -> TokenAsterisk : lex rest
    '$' : rest -> TokenDollar : lex rest
    '^' : rest -> TokenCaret : lex rest
    '+' : rest -> TokenPlus : lex rest
    '.' : rest -> TokenDot : lex rest
    '?' : rest -> TokenQuestionMark : lex rest
    '(' : rest -> TokenParenthesisOpen: lex rest
    ')' : rest -> TokenParenthesisClose : lex rest
    '\\' : c : rest -> Token c : lex rest
    c : rest -> Token c : lex rest

-- ============================================
-- Parsing
-- ============================================

data Parser a = Parser { parseTokens :: [Token] -> [(a, [Token])] }

parse :: Parser a -> [Token] -> Maybe a
parse (Parser f) tokens =
  case f tokens of
    [] -> Nothing
    [(r,_)] -> Just r
    _ -> error "ambiguous parse"

instance Semigroup a => Semigroup (Parser a) where
  (<>) :: Parser a -> Parser a -> Parser a
  (Parser f) <> (Parser g) =
    Parser $ \tokens -> do
      (r1, rest1) <- f tokens
      (r2, rest2) <- g rest1
      return (r1 <> r2, rest2)

instance Monoid a => Monoid (Parser a) where
  mempty :: Parser a
  mempty = Parser $ \tokens -> [(mempty, tokens)]

  mappend :: Parser a -> Parser a -> Parser a
  mappend = (<>)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser g) = Parser $ \tokens -> do
    (r, rest) <- g tokens
    return (f r, rest)

instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> g = Parser $ \tokens -> do
    (f', rest1) <- parseTokens f tokens
    (a, rest2) <- parseTokens g rest1
    pure (f' a, rest2)

  pure :: a -> Parser a
  pure x = Parser $ \tokens -> [(x, tokens)]

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a
  f <|> g = Parser $ \tokens ->
    case parseTokens f tokens of
      [] -> parseTokens g tokens
      xs -> xs

  empty :: Parser a
  empty = Parser $ \_ -> []

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser f) >>= g = Parser $ \tokens -> do
    (r, rest) <- f tokens
    parseTokens (g r) rest

instance MonadFail Parser where
  fail _ = Parser $ \_ -> []

regexParser :: Parser Regex
regexParser = parseAnds

chomp :: Parser Token
chomp = Parser $ \tokens ->
  case tokens of
    [] -> []
    x : xs -> [(x, xs)]

parseAnds :: Parser Regex
parseAnds = do
  rs <- many parseWithPostfix
  case rs of
    [] -> empty
    [x] -> pure x
    x : xs -> pure $ foldr (flip And) x xs

parseWithPostfix :: Parser Regex
parseWithPostfix = do
  r <- parseBlock
  postfixFor r <|> pure r
  where
    postfixFor regex = do
      token <- chomp
      case token of
        TokenPlus -> pure (Plus regex)
        TokenAsterisk -> pure (Asterisk regex)
        TokenQuestionMark -> pure (Question regex)
        _ -> empty

parseBlock :: Parser Regex
parseBlock = do
  token <- chomp
  case token of
    TokenCaret -> pure MatchStart
    Token chr -> pure (MatchChar chr)
    TokenDollar -> pure MatchEnd
    TokenDot -> pure MatchAny
    TokenParenthesisOpen -> do
      r <- parseAnds
      TokenParenthesisClose <- chomp
      pure r
    _ -> empty
