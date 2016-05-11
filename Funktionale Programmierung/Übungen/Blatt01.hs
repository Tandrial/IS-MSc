{-# OPTIONS_GHC -Wall -Werror #-}

module Blatt01 where

-- Aufgabe 1.1

summe :: (Num a) => (a, a) -> a
summe (x, y) = x + y

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y
-- uncurry f = \(x, y) -> f x y

-- Aufgabe 1.2

minus :: (Num a) => a -> a -> a
minus x y = x - y

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

strange :: ((a -> b) -> a) -> (a -> b) -> b
strange f g = g (f g)

{- Nicht möglich
stranger f = f f

linkes f              | rechtes f
(a -> b)              | a
-}

-- Aufgabe 1.3
{-

f :: c -> d
g :: a -> (b -> c)

h :: a -> b -> d
h x y = f (g x y)

h = f . g Geht nicht da g als Return type ( b -> c) liefert, f aber c haben will
h x = f . (g x) Geht
h x y = (f . g) x y Klammern haben vorrand, also selbes Problem wie 1.

-}

-- Aufgabe 1.4

bar ::  Int -> Int
bar x = x + 1

sum1 :: (Int -> Int) -> Int -> Int -> Int
sum1 f a b
  | a > b = 0
  | otherwise = f a + sum1 f (a + 1) b

sum2 :: (Int -> Int -> Int) -> Int -> Int -> Int
sum2 f n m = sum1 (\i -> sum1 (f i) 0 m) 0 n

sum3 :: (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int
sum3 f o n m = sum1 (\k -> sum2 (f k) n m) 0 o

-- Aufgabe 1.5
fibo :: Int -> Int
fibo n
  | n < 2 = n
  | otherwise = fibo (n - 1) + fibo (n - 2)