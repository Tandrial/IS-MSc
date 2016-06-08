{-# OPTIONS_GHC -Wall -Werror #-}

module Blatt08 where

import Control.Monad (liftM, ap)

{-
-- Aufgabe 8.1

	instance Monad Exc where
	-- return :: a -> Exc a
	  return = Return
	-- (>>=) :: Exc a -> (a -> Exc b) -> Exc b
	  (Raise e)  >>= q = Raise e
	  (Return x) >>= q = q x

  (1) p >>= return = p

      Fall (Raise e): zu Zeigen: (Raise e) >>= return = (Raise e)

        LHS: (Raise e) >>= return   | 1.Regel >>=
          <=>(Raise e)

        RHS: (Raise e)

      Fall (Return x): zu Zeigen: (Return x) >>= return = (Return x)

        LHS: (Return x) >>= return  | 2.Regel >>=
          <=>(return x)             | 1.Regel return
          <=>(Return x)

        RHS: (Return x)


  (2) return e >>= q = q e

      Fall (Raise e): zu Zeigen: return (Raise e) >>= q = q (Raise e)

        LHS: return (Raise e) >>= q     | 1.Regel return
          <=>(Return (Raise e)) >>= q   | 2. Regel >>=
          <=>q (Raise e)

        RHS: q (Raise e)

      Fall (Return x): zu Zeigen: return (Return x) >>= q = q (Return x)

        LHS: return (Return x) >>= q    | 1. Regel return
          <=>(Return (Return x)) >>= q  | 2. Regel >>=
          <=>q (Return x)

        RHS: q (Return x)


  (3) (p >>= q) >>= r = p >>= (\x -> q x >>= r)

  		Fall (p = (Raise p) UND q, r = egal): zu Zeigen: ((Raise p) >>= q) >>= r = (Raise p) >>= (\x -> q x >>= r)

        LHS: ((Raise p) >>= q) >>= r                   | 1. Regel >>=
        	<=>(Raise p) >>= r                           | 1. Regel >>=
        	<=>(Raise p)

        RHS: (Raise p) >>= (\x -> q x >>= r)           | 1. Regel >>=
        	<=>(Raise p)

      Fall (p = (Return p) UND q = (Raise q) UND r = egal): zu Zeigen: ((Return p) >>= (Raise q)) >>= r = (Return p) >>= (\x -> (Raise q) x >>= r)

        LHS: ((Return p) >>= (Raise q)) >>= r          | 2. Regel >>=
          <=>((Raise q) p) >>= r                       | Klammern entfernen
          <=>(Raise q) p >>= r

        RHS: (Return p) >>= (\x -> (Raise q) x >>= r)  | 2. Regel >>=
        	<=>(\x -> (Raise q) x >>= r) p               | Application
        	<=>(Raise q) p >>= r

      Fall (p = (Return p) UND q = (Return q) UND r = egal): zu Zeigen: ((Return p) >>= (Return q)) >>= r = (Return p) >>= (\x -> (Return q) x >>= r)

        LHS: ((Return p) >>= (Return q)) >>= r         | 2. Regel >>=
        	<=>((Return q) p) >>= r                      | Klammern entfernen
        	<=>(Return q) p >>= r

        RHS: (Return p) >>= (\x -> (Return q) x >>= r) | 2. Regel >>=
        	<=>(\x -> (Return q) x >>= r) p              | Application
        	<=>(Return q) p >>= r

-}

-- Aufgabe 8.2

data Sum a = MkSum (a, Int)
  deriving (Show)

instance Functor Sum where
  fmap = liftM

instance Applicative Sum where
  pure  = return
  (<*>) = ap

instance Monad Sum where
  -- return :: a -> Sum a
  return a = MkSum (a, 0)
  -- (>>=) :: Sum a -> (a -> Sum b) -> Sum b
  MkSum(value, cnt) >>= q = MkSum (value', cnt + cnt')
    where MkSum (value', cnt') = q value

data Term = Con Int | Div Term Term
            deriving Show

answer :: Term
answer =  Div (Div (Con 120) (Con 5)) (Con 2)

evalSum :: Term -> Sum Int
evalSum (Con x ) = return x
evalSum (Div t u) = do
                      x <- evalSum t
                      y <- evalSum u
                      inc $ return (x `div` y)

inc :: Sum a -> Sum a
inc (MkSum (a, cnt)) =  MkSum (a, cnt + 1)
