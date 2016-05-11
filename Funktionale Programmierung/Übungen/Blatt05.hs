{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt05 where

import Blatt04
import Data.List

-- \\ mit fold
listDiffL :: Eq a => [a] -> [a] -> [a]
listDiffL = foldl (flip delete)

listDiffR :: Eq a => [a] -> [a] -> [a]
listDiffR = foldr delete

-- Aufgabe 5.1
{- [r,s,t,u,v,w,x,y,z,_]
 = [8,1,2,6,7,5,4,0,3,9]
 a  -  b  =  c     rsr - tuv = wws    818 - 267 = 551
 -     +     -      -     +     -      -     +     -
 d  +  e  =  f ==> txy + sss = zws => 240 + 111 = 351
 =     =     =      =     =     =      =     =     =
 g  -  h  =  i     wvr - zvr = tyy    578 - 378 = 200
-}

solutions :: [Int]
solutions = head [p | p <- perms [0..9],
  let [r, s, t, u, v, w, x, y, z, _] = p
      [a, b, c] = map bNum [[r, s, r], [t, u, v], [w, w, s]]
      [d, e, f] = map bNum [[t, x, y], [s, s, s], [z, w, s]]
      [g, h, i] = map bNum [[w, v, r], [z, v, r], [t, y, y]]
  in y == 0 && a - b == c && d + e == f && g - h == i
            && a - d == g && b + e == h && c - f == i]

bNum :: [Int] -> Int
bNum = foldl (\x y -> x * 10 + y) 0

-- Aufgabe 5.2

mergesort :: Ord a => [a] -> [a]
mergesort []  = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort l1) (mergesort l2)
  where (l1, l2) = splitAt (div (length xs) 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- Aufgabe 5.3

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x y -> if f x then x:y else y) []

-- Aufgabe 5.4

remdupsL :: Eq a => [a] -> [a]
remdupsL = foldl cmpLast []
  where cmpLast [] y = [y]
        cmpLast xs y = if (last xs) == y then xs else xs ++ [y]

remdupsR :: Eq a => [a] -> [a]
remdupsR = foldr cmpHead []
  where cmpHead x []       = [x]
        cmpHead x ys@(y:_) = if (x == y) then ys else x:ys

-- Aufgabe 5.5

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (x:xs) = [y:cs | y <- x, cs <- (cp xs)]

-- Aufgabe 5.6

{-
foldr :: (a -> b -> b) -> b -> a -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

Sei f strikt, f a = b und f (g x y) = h x (f y) für alle x und y.
Dann gilt: f · foldr g a     = foldr h b
       ==> f . foldr g a $ p = foldr h b p
       ==> f (foldr g a p)   = foldr h b p

Fall(bottom): zu Zeigen: f . foldr g a $ bottom = foldr h b bottom

  LHS: f (foldr g a bottom)   [foldr ist strikt im 3. Argument]
    <=>f (bottom)             [f ist strikt]
    <=> bottom

  RHS: foldr h b bottom       [foldr ist strikt im 3. Argument]
    <=>bottom

Fall([]): zu Zeigen: f . foldr g a $ [] = foldr h b []

  LHS: f (foldr g a [])       [foldr 1. Regel]
    <=>f (a)                  [f     1. Regel]
    <=>b

  RHS: foldr h b []           [foldr 1. Regel]
    <=>b

Fall(x:xs): zu Zeigen: f . foldr g a $ (x:xs) = foldr h b (x:xs)
  Annahme: Aussage ist korrekt für xs € [a]

  LHS: f (foldr g a (x:xs))   [foldr 2. Regel]
    <=>f (g x (foldr g a xs)) [f     2. Regel]
    <=>h x (f (foldr g a xs)) [Indu Hypothese]
    <=>h x (foldr h b xs)

  RHS: foldr h b (x:xs)       [foldr 2. Regel]
    <=>h x (foldr h b xs)
-}
