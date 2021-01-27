-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
import Data.Foldable
import ParseState
import Result

import Control.Monad.State

-- Basic definitions

newtype Parser a = P (ParseState -> (Maybe a, ParseState))

parseToResult :: Parser a -> String -> Result a
parseToResult p inp = case parse p $ parseState inp of
                          (Nothing, ps) -> err ps
                          (Just a, ps)  -> if hasNextChar ps then err ps else Result a
                        where err ps = Error $ "Malformed token at " ++ (showPos ps)

parse :: Parser a -> ParseState -> (Maybe a, ParseState)
parse (P p) s = p s

item :: Parser Char
item = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, inc s)
                else
                    (Nothing, s))

-- Fails out if no next token
peek :: Parser Char
peek = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, s)
                else
                    (Nothing, s))

canPeek :: Parser Bool
canPeek = P (\s -> (Just $ hasNextChar s, s))

-- Attempts to peek without failing out if non left
tryPeek :: Parser (Maybe Char)
tryPeek = do able <- canPeek
             if able then fmap Just peek else return Nothing

-- ParseState getter
getState :: Parser ParseState
getState = P (\s -> (Just s, s))

-- Ensures a Parser fails to succeed, essentially flipping behaviour
mustFail :: Parser a -> Parser ()
mustFail p = P (\s -> case parse p s of
                          (Nothing, s') -> (Just (), s)
                          (Just _, s')  -> (Nothing, s))

-- Sequencing parsers

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\s -> case parse p s of
                            (Nothing, s') -> (Nothing, s')
                            (Just v, s')  -> (Just (g v), s'))

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\s -> (Just v, s))

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\s -> case parse pg s of
                             (Nothing, s') -> (Nothing, s')
                             (Just g, s')  -> parse (fmap g px) s')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\s -> case parse p s of
                           (Nothing, s') -> (Nothing, s)
                           (Just v, s')  -> parse (f v) s')

-- Making choices

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\s -> (Nothing, s))

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\s -> case parse p s of
                           (Nothing, _) -> parse q s
                           (Just v, s') -> (Just v, s'))

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Matches any of the strings in given list
anyString :: [String] -> Parser String
anyString xs = asum $ map string xs

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

upperIdent :: Parser String
upperIdent = do x  <- upper
                xs <- many alphanum
                return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

float :: Parser Float
float = do n <- int
           char '.'
           d <- some digit
           return $ read $ show n ++ '.':d
         <|> fmap fromIntegral int
