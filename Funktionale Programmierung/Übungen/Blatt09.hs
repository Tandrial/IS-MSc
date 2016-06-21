{-# OPTIONS_GHC -Wall -Werror #-}

module Blatt09 where

import Lambda        (LambdaTerm(Abs, App, Var), hasRedex, subst)
import Parser        (Parser(), applyParser, orelse, ident, token, symbol)
import Blatt06_Taut  (Prop(Atom, Conj, Disj, Neg), show2)
import Control.Monad (liftM, ap)

-- Aufgabe 9.1

type Map  = [(String, Integer)]
data St a = MkSt (Map -> (a , Map))

instance Show a => Show (St a) where
  show (MkSt f) = show x ++ " : " ++ show s
    where (x, s) = f []

instance Functor St where
  fmap = liftM

instance Applicative St where
  pure  = return
  (<*>) = ap

instance Monad St where
  -- return :: a -> St a
  return x = MkSt f
    where f s = (x, s)
  -- (>>=) :: St a -> (a -> St b) -> St b
  (MkSt p) >>= q = MkSt f
    where f s = apply (q x) s'
      where (x, s') = p s

apply :: St a -> Map -> (a, Map)
apply (MkSt f) s = f s

conc :: [String] -> St String
conc [] = return ""
conc (x:xs) = do ele  <- return x
                 _    <- merge x
                 rest <- conc xs
                 return (ele ++ rest)

merge :: String -> St String
merge x = MkSt(\m -> (x, merge' m))
  where merge' []     = [(x, 1)]
        merge' (y:ys) = if (x == fst y) then (x, 1 + snd y) : ys else y : merge' ys

-- Aufgabe 9.2

{-
  <prop>     ::= <term> <restProp>
  <restProp> ::= '|' <term> <restProp>   | nothing
  <term>     ::= <factor> <restTerm>
  <restTerm> ::= '&' <factor> <restTerm> | nothing

  <factor>   ::= <var> | '~'<factor> | '(' <prop> ')'
  <var>      ::= [a-z]+
-}

parserProp :: Parser Prop
parserProp = do {t <- parserTerm; restProp t}

parserTerm :: Parser Prop
parserTerm = do {t <- parserFactor; restTerm t}

restProp :: Prop -> Parser Prop
restProp t = do {symbol "|"; t' <- parserTerm; restProp (Disj t t')}
    `orelse` do return t

restTerm :: Prop -> Parser Prop
restTerm t = do {symbol "&"; t' <- parserFactor; restTerm (Conj t t')}
    `orelse` do return t

parserFactor :: Parser Prop
parserFactor = do { t <- token ident; return (Atom t) }
      `orelse` do {symbol "~"; t <- parserFactor; return (Neg t)}
      `orelse` do {symbol "("; t <- parserProp; symbol ")"; return t}

testParser :: Bool
testParser = and . map (\s -> s == pTest s) $ (test1 ++ test2 ++ test3)
  where pTest = show2 . applyParser parserProp
        test1 = ["a", "~a", "a | b", "a & b", "~a | b", "a | ~b" , "~a | ~b", "~(~a | b)", "~(a & ~b)"]
        test2 = ["a | b | c", "a & b & c", "a | ~b | ~c", "a & ~b & ~c", "~a & (~b | c)", "a | ~b & c"]
        test3 = ["a & ~b & c | ~a & b & ~c | ~a & b & c | ~a | b", "~((~a | b) & ~(b & ~c)) | ~a | ~c"]

-- Aufgabe 9.3

applicativeReduce :: LambdaTerm -> LambdaTerm
applicativeReduce (Var t)           = Var t
applicativeReduce (Abs x t)         = Abs x (applicativeReduce t)
applicativeReduce (App (Abs x t) n) = if hasRedex t
                                        then App (Abs x (applicativeReduce t)) n
                                        else subst n x t
applicativeReduce (App t t')        = if hasRedex t
                                        then App (applicativeReduce t) t'
                                        else if hasRedex t'
                                          then App t (applicativeReduce t')
                                          else App t t'

-- Aufgabe 9.4

{-
data St2 a = MkSt2 (Int -> Int , a)

instance Monad St2 where
  -- return :: a -> St2 a
  return x = MkSt2 (id, x)
  -- (>>=) :: St2 a -> (a -> St2 b) -> St2 b
  (MkSt2 (f, x)) >>= q = MkSt2 (g . f, y)
                       where MkSt2 (g, y) = q x

  (1) p >>= return = p

    zu Zeiten:  MkSt2 (f, a) >>= return = MkSt2 (f, a)
      mit  f :: Int -> Int

      LHS: MkSt2 (f, a) >>= return                              | Regel >>=
        <=>MkSt2 (g . f, y) mit MkSt2 (g, y) = return a         | Regel return
        <=>MkSt2 (g . f, y) mit MkSt2 (g, y) = MkSt2 (id, a)    | Ersetzen von g und y
        <=>MkSt2 (id . f, a)                                    | id . f <=> f
        <=>MkSt2 (f, a)

      RHS: MkSt2 (f, a)

  (2) return e >>= q = q e

    zu Zeigen: return e >>= MkSt2 (f, a) = MkSt2 (f, a) e
      mit f :: Int -> Int UND :t a == :t e

      LHS: return e >>= MKSt2 (f, a)                           | Regel return
        <=>MkSt2 (id, e) >>= MKSt2 (f, a)                      | Regel >>=
        <=>MKSt2 (g . id, y) mit MkSt2 (g, y) = MkSt2 (f, a) e | g . id <=> g
        <=>MkSt2 (g, y) mit MkSt2 (g, y) = MkSt2 (f, a) e      | Ersetzen MkSt2 (g, y)
        <=>MkSt2 (f, a) e

      RHS: MkSt2 (f, a) e
-}
