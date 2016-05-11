{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt02 where

import Data.Char

-- Aufgabe 2.1

data Punkt = MkPt Double Double
  deriving (Eq)

instance Show Punkt where
  show (MkPt x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Rechteck = MkRect Punkt Double Double
  deriving (Eq)

instance Show Rechteck where
  show (MkRect p b h) = "[Ursprung: " ++ show p
                     ++ ", Breite: "  ++ show b
                     ++ ", Hoehe: "   ++ show h ++ "]"

huelle :: Rechteck -> Rechteck -> Rechteck
huelle (MkRect (MkPt p1X p1Y) b1 h1) (MkRect (MkPt p2X p2Y) b2 h2) =
  (MkRect (MkPt x y) b h)
  where x = min p1X p2X
        y = min p1Y p2Y
        b = max (p1X + b1) (p2X + b2) - x
        h = max (p1Y + h1) (p2Y + h2) - y

-- Aufgabe 2.2

wert :: Integer -> [Char] -> Integer
wert _ [] = 0
wert b (x:xs)
  | b < 2    || b   > 10  = error "Base must be between 2 and 10!"
  | b <= val || val <  0  = error ([x] ++ " is too big for base " ++ show b)
  | otherwise             = val * b ^ (length xs) + wert b xs
  where val = toInteger (ord x - ord '0')

value :: Integer -> [Char] -> Integer
value basis = snd . (value' basis)

value' :: Integer -> [Char] -> (Integer, Integer)
value' _ [] = (0, 0)
value' basis (z:zn) = (basis * faktor, faktor * toInteger(ord z - ord '0') + wertRest)
  where (faktor, wertRest) = value' basis zn

repr :: Integer -> Integer -> Integer
repr b x
  | b < 2 || b > 10 = error "Base must be between 2 and 10!"
  | b > x           = x
  | otherwise       = 10 * repr b (div x b) + (mod x b)

summe :: Integer -> ([Char], [Char]) -> Integer
summe b = repr b . uncurry (+) . pair' (wert b)

repr' :: Integer -> Integer -> [Char]
repr' b x
  | b < 2 || b > 10 = error "Base must be between 2 and 10!"
  | b > x           = show x
  | otherwise       = repr' b (div x b) ++ show (mod x b)

summe' :: Integer -> ([Char], [Char]) -> [Char]
summe' b = repr' b . uncurry (+) . pair' (wert b)

pair' :: (a -> b) -> (a, a) -> (b, b)
pair' f (x,y) = (f x, f y)

-- Aufgabe 2.3

{-
cross (f, g) . cross (h, k) = cross(f . h, g . k)

   cross (f, g) . cross (h, k)                            [Def Cross]
<=>cross (f, g) . pair (h . fst, k . snd)                 [#4]
<=>pair  (f . h . fst, g . k . snd)                       [Def Cross]
<=>cross (f . h, g . k)

case (f, g) . plus (h, k) = case (f . h, g . k)

   case (f, g) . plus (h, k)                              [Def plus]
<=>case (f, g) . case (Left . h, Right . k)               [#3]
<=>case (case (f, g) . Left . h, case (f, g) . Right . k) [#1 + #2]
<=>case (f . h, g . k)
-}
