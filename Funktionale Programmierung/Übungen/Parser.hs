{-# OPTIONS_GHC -Wall -Werror #-}

module Parser where

import Control.Monad (liftM, ap)
import Data.Char

data Parser a = MkP (String -> [(a, String)])

apply         :: Parser a -> String -> [(a, String)]
apply (MkP f) =  f

applyParser   :: Parser a -> String -> a
applyParser p =  fst . head . (apply p)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Monad Parser where
-- return   :: a -> Parser a
   return x =  MkP (\s -> [(x, s)])
-- (>>=)   :: Parser a -> (a -> Parser b) -> Parser b
   p >>= q =  MkP f
              where f s = [(y, s'') | (x, s') <- apply p s, (y, s'') <- apply (q x) s']


-- Grundlegende Parser
-- ===================

-- Liefert das erste Zeichen der Eingabe, schlägt bei leerer Eingabe fehl.
item :: Parser Char
item =  MkP f
        where f []       = []
              f (c : cs) = [(c, cs)]

-- Liefert erstes Zeichen der Eingabe, wenn es eine bestimmte Bedingung erfüllt.
sat   :: (Char -> Bool) -> Parser Char
sat p = do
          c <- item
          if p c then return c else MkP (const [])

-- Schlägt fehl, wenn Eingabe nicht mit einem bestimmten Zeichen beginnt.
char   :: Char -> Parser ()
char x =  do
            _ <- sat (== x)
            return ()

-- Schlägt fehl, wenn Eingabe nicht mit einer bestimmten Zeichenfolge beginnt.
string          :: String -> Parser ()
string []       =  return ()
string (x : xs) =  do
                     char x
                     string xs

-- Liefert nächstes Zeichen der Eingabe, wenn es ein Kleinbuchstabe ist.
lower :: Parser Char
lower =  sat isLower


-- Alternative Parser
-- ==================

-- Liefert Parse-Ergebnisse des ersten Parsers, falls dieser nicht fehlschlägt,
-- sonst Parse-Ergebnisse des zweiten Parsers.
orelse       :: Parser a -> Parser a -> Parser a
p `orelse` q =  MkP f
                where f s = if null ps then apply q s else ps
                            where ps = apply p s


-- Wiederholung
-- ============

-- Wiederholt einen Parser, bis er fehlschlägt.
many   :: Parser a -> Parser [a]
many p =  (do
             x  <- p
             xs <- many p
             return (x : xs))
          `orelse` return []

-- Liefert einen Identifier.
ident :: Parser String
ident =  do
           c  <- lower
           cs <- many (sat (\x -> isLower x || isDigit x))
           return (c : cs)

-- White space
-- ===========

-- Konsumiert white space.
space :: Parser ()
space =  do
           _ <- many (sat isSpace)
           return ()

-- Konsumiert white space und liefert dann, was übergebener Parser liefert.
token :: Parser a -> Parser a
token p =  do
             space
             x <- p
             space
             return x

-- Konsumiert white space und übergebene Zeichenkette.
symbol    :: String -> Parser ()
symbol xs =  token (string xs)
