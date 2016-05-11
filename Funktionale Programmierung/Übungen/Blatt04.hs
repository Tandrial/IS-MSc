{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt04 where

import Data.List

-- Aufgabe 4.1

takewhile :: (a -> Bool) -> [a] -> [a]
takewhile _ [] = []
takewhile p (x:xs) = if p x then x : takewhile p xs else []

-- Aufgabe 4.2

pairs :: Integral a => a -> [(a, a)]
pairs n = [(x, y) | x <- [1..n], y <- [1..n]]

-- Aufgabe 4.3

perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ps | x <- xs, ps <- perms (delete x xs)]

-- Aufgabe 4.4

a44 :: Ord a => [a] -> [(a,a)]
a44 xs = filter (uncurry (<)) . zip xs $ tail xs

-- Aufgabe 4.5

{-
  (++) :: [a] -> [a] -> [a]
  []     ++ ys = ys
  (x:xs) ++ ys = x:(xs ++ ys)

  alle xs € [a] gilt : xs ++ [] = xs

  Fall(bottom): bottom ++ [] = bottom
   LHS: bottom ++ []
     <=>bottom

   RHS: bottom

  Fall([]): [] ++ [] = []
   LHS: [] ++ []      [1. Regel]
     <=>[]

   RHS: []

  Fall(x:xs): (x:xs) ++ [] = (x:xs)
   Annahme: Aussage ist korrekt für xs € [a]
   LHS: (x:xs) ++ []    [2. Regel]
       <=>x:(xs ++ [])  [Induktions Hypo]
       <=>(x:xs)

   RHS: (x:xs)

-- Aufgabe 4.6

map         :: (a -> b) -> [a] -> [b]
Map hebt eine Funktion f, welche mit a und b arbeite auf eine Funktion die mit
[a] und [b] arbeitet

map map :: [a'] -> [b']
a' = (a -> b)   [Parameter von map]
b' = [a] -> [b] [Rückgabetyp von map]

map map :: [a -> b] -> [[a] -> [b]]

ist eine Funktion die eine Liste von Funktionen mit a -> b entgegen nimmt und
in eine Liste von Funktionen mit [a] -> [b] umwandelt
-}
