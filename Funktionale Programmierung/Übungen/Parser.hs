{-# LANGUAGE NPlusKPatterns #-}
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

-- Parser, der immer fehlschlägt
zero :: Parser a
zero =  MkP (const [])

-- Liefert erstes Zeichen der Eingabe, wenn es eine bestimmte Bedingung erfüllt.
sat   :: (Char -> Bool) -> Parser Char
sat p = do
          c <- item
          if p c then return c else zero

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

-- Liefert Ziffernwert, wenn nächstes Zeichen der Eingabe eine Ziffer ist.
digit :: Parser Int
digit =  do
           d <- sat isDigit
           return (ord d - ord '0')



-- Alternative Parser
-- ==================

-- Vereinigung der Parse-Ergebnisse zweier Parser
plus :: Parser a -> Parser a -> Parser a
p `plus` q = MkP f
             where f s = apply p s ++ apply q s

-- Liefert Anfangsfolge aller Kleinbuchstaben.
lowers :: Parser String
lowers =  (do
             c  <- lower
             cs <- lowers
             return (c : cs))
          `plus` (return "")

-- Liefert Parse-Ergebnisse des ersten Parsers, falls dieser nicht fehlschlägt,
-- sonst Parse-Ergebnisse des zweiten Parsers.
orelse       :: Parser a -> Parser a -> Parser a
p `orelse` q =  MkP f
                where f s = if null ps then apply q s else ps
                            where ps = apply p s

lowers' :: Parser String
lowers' =  (do
              c  <- lower
              cs <- lowers'
              return (c : cs))
           `orelse` return ""


-- Wiederholung
-- ============

-- Wiederholt einen Parser, bis er fehlschlägt.
many   :: Parser a -> Parser [a]
many p =  (do
             x  <- p
             xs <- many p
             return (x : xs))
          `orelse` return []

-- Es gilt: lowers' = many lower

-- Liefert einen Identifier.
ident :: Parser String
ident =  do
           c  <- lower
           cs <- many (sat (\x -> isLower x || isDigit x))
           return (c : cs)

-- Wiederholt einen Parser, bis er fehlschlägt, mindestens aber 1 mal.
some   :: Parser a -> Parser [a]
some p =  do
            x  <- p
            xs <- many p
            return (x : xs)

-- Liefert vorzeichenlose Zahl, mit der Eingabe beginnt.
nat :: Parser Int
nat =  do
         ds <- some digit
         return (foldl conv 0 ds)
                 where m `conv` n = 10 * m + n

-- Liefert positive oder negative Zahl, mit der Eingabe beginnt.
int :: Parser Int
int =  (do
          char '-'
          n <- nat
          return (-n))
       `orelse` nat



-- Wiederholung mit Trennzeichen
-- =============================

-- Liefert Liste von Zahlen, wenn Eingabe mit [n,m,...] beginnt.
ints :: Parser [Int]
ints =  do
          char '['
          i  <- int
          is <- many (do {char ','; int})
          char ']'
          return (i : is)

-- Verallgemeinerung von ints, wobei Trennung durch Sequenz erfolgt,
-- die der Parser skip erkennt.
somewith        :: Parser b -> Parser a -> Parser [a]
somewith skip p =  do
                     x  <- p
                     xs <- many (do {_ <- skip; p})
                     return (x : xs)

-- wie somewith, aber auch mit 0 Wiederholungen
manywith        :: Parser b -> Parser a -> Parser [a]
manywith skip p =  somewith skip p `orelse` return []



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
