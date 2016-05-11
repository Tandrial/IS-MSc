{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt03 where

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

instance Enum Nat where
  fromEnum = foldn (+1) 0
  toEnum 0 = Zero
  toEnum n = Succ (toEnum (n - 1))

abl :: Double -> (Double -> Double) -> Double -> Double
abl eps f x0 = abl' d0 s0
  where s0 = (f(x0 + d0) - f(x0 - d0)) / (2 * d0)
        d0 = 0.001
        abl' d s = if (abs(s - s') < eps) then s else abl' (d / 2) s'
          where s' = (f(x0 + d / 2) - f(x0 - d / 2)) / d

-- Aufgabe 3.1

foldi :: (a -> a) -> a -> Integer -> a
foldi _ c 0 = c
foldi h c n = h (foldi h c (n - 1))

--abl_n :: Double -> Integer -> (Double -> Double) -> Double -> Double
--abl_n = flip . foldi . abl
--Herleitung
--abl_n eps n f x0 = (foldi (abl eps) f n) x0
--abl_n eps n f    = foldi (abl eps) f n
--abl_n eps        = flip (foldi (abl eps))
--abl_n eps        = flip . foldi . abl $ eps

abl_n :: Double -> (Double -> Double) -> Integer -> Double -> Double
abl_n = foldi . abl

-- Aufgabe 3.2

foldn :: (a -> a) -> a -> Nat -> a
foldn _ c   Zero   = c
foldn h c (Succ n) = h (foldn h c n)

minusn :: Nat -> Nat -> Nat
minusn = foldn remSucc
  where remSucc Zero = Zero
        remSucc (Succ e) = e

-- Aufgabe 3.3

minN :: Nat -> Nat -> Nat
minN Zero _ = Zero
minN (Succ _) Zero = Zero
minN (Succ m) (Succ n) = Succ(minN m n)

infinity :: Nat
infinity =  Succ infinity

{-
  1. Minimum-Funktion

  2. alle m € Nat : m `minN` infinity = m

    Fall (Bottom): bottom `minN` infinity = bottom, keine Regel anwendbar
                                                    min strikt 1. Argument

    Fall (Zero): Zero `minN` infinity = Zero, siehe 1. Regel

    Fall (Succ m): (Succ m) `minN` infinity = (Succ m)
      Annahme: Aussabe ist korretk für m € Nat

        LHS: (Succ m) `minN` infinity        [eval infinity]
          <=>(Succ m) `minN` (Succ infinity) [3. Regel]
          <=>(Succ (m `minN` infinity))      [Indu Hyp]
          <=>(Succ m)

        RHS: (Succ m)

  3. alle m € Nat : infinity `minN` m = m

    Fall (Bottom): infinity `minN` bottom = bottom, keine Regel anwendbar

    Fall (Zero): infinity `minN` Zero = Zero

        LHS: infinity `minN` Zero            [eval infinity]
          <=>(Succ infinity) `minN` Zero     [2. Regel]
          <=>Zero

        RHS: Zero

    Fall (Succ m): infinity `minN` (Succ m) = (Succ m)

        LHS: infinity `minN` (Succ m)        [eval infinity]
          <=>(Succ infinity) `minN` (Succ m) [3. Regel]
          <=>(Succ (infinity `minN` m))      [Indu Hyp]
          <=>(Succ m)

        RHS: (Succ m)

  4. m `minN` n == n `minN` m

    m = undefined n = Zero ==>  undefined `minN` Zero = undefined [keine Regel anwendbar]
                                Zero `minN` undefined = Zero      [1. Regel]

    Nicht kommutativ mit Bottom. Ohne Bottom:

    Fall (Zero1) m = (Zero), n = (Zero):
        zu Zeigen (Zero) `minN` (Zero) == (Zero) `minN` (Zero)

    Fall (Zero2) m = (Succ x), n = (Zero) ODER m = (Zero), n = (Succ x):
        zu Zeigen: (Succ x) `minN` (Zero) == (Zero) `minN` (Succ x)

        LHS: (Succ x) `minN` (Zero)          [2. Regel]
          <=>(Zero)

        RHS: (Zero) `minN` (Succ x)          [1. Regel]
          <=>(Zero)

    Fall (Succ) m = (Succ x), n = (Succ y):
        zu Zeigen: (Succ x) `minN` (Succ y) == (Succ y) `minN` (Succ x)

        LHS: (Succ x) `minN` (Succ y)        [3. Regel]
          <=>(Succ (x `minN` y))             [3. wiederholen bis x or y == Zero]

          Fall i) x = Zero AND y = Zero
          <=>(Zero) `minN` (Zero)            [1. Regel]
          <=>(Zero)

          Fall ii) x = Zero
          <=>(Succ (Zero `minN` y'))         [1. Regel]
          <=>(Succ (Zero)) {x mal}
          <=>x

          Fall iii) y = Zero
          <=>(Succ (x' `minN` Zero))         [2. Regel]
          <=>(Succ (Zero)) {y mal}
          <=>y

        RHS: (Succ y) `minN` (Succ x)        [3. Regel]
          <=>(Succ (y `minN` x))             [3. wiederholen bis x or y == Zero]

          Fall i) x = Zero AND y = Zero
          <=>(Zero) `minN` (Zero)            [1. Regel]
          <=>(Zero)

          Fall ii) x = Zero
          <=>(Succ (y' `minN` Zero))         [1. Regel]
          <=>(Succ (Zero)) {x mal}
          <=>x

          Fall iii) y = Zero
          <=>(Succ (Zero `minN` x'))         [2. Regel]
          <=>(Succ (Zero)) {y mal}
          <=>y
-}
