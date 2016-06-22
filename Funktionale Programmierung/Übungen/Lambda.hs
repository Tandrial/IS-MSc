{-# LANGUAGE NPlusKPatterns #-}

module Lambda where

import Data.Char
import Data.Set
import Data.Maybe


-- Datentyp für Lambda-Terme
-- =========================

data LambdaTerm = Var String
                | Abs String LambdaTerm
                | App LambdaTerm LambdaTerm
                deriving Eq

instance Show LambdaTerm where
  show (Var s)                        = s
  show (a@(Abs x t))                  = "/" ++ showAbs a
  show (App t@(Abs _ _) t'@(Abs _ _)) = "(" ++ show t ++ ")(" ++ show t' ++ ")"
  show (App t@(Abs _ _) t')           = "(" ++ show t ++ ")" ++ show t'
  show (App t t'@(Abs _ _))           = show t ++ "(" ++ show t' ++ ")"
  show (App t t'@(App _ _))           = show t ++ "(" ++ show t' ++ ")"
  show (App t t')                     = show t ++ show t'

showAbs :: LambdaTerm -> String
showAbs (Abs x t@(Abs _ _)) = x ++ showAbs t
showAbs (Abs x t)           = x ++ "." ++ show t


-- String-Darstellung von Lambda-Termen mit vollständiger Klammerung
-- =================================================================

show1 :: LambdaTerm -> String
show1 (Var s)   = s
show1 (App f t) = "(" ++ show1 f ++ show1 t ++ ")"
show1 (Abs x t) = "(/" ++ x ++ show1 t ++ ")"


-- Substitution
-- ============

-- bestimmt freie Variablen eines Lambda-Terms
fv           :: LambdaTerm -> Set String
fv (Var s)   =  singleton s
fv (App f t) =  (fv f) `union` (fv t)
fv (Abs x t) =  delete x (fv t)


-- freie Vorkommen von x in Lambda-Term durch n ersetzen
subst                 :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
subst n x (Var y)     =  if x == y then n else (Var y)
subst n x (App m1 m2) =  App (subst n x m1) (subst n x m2)
subst n x (Abs y m)   =  if x == y
                           then (Abs y m)
                           else Abs z (subst n x (subst (Var z) y m))
                         where z        = newVar y
                               newVar y = if not (y `member` fvs)
                                            then y
                                            else newVar (nextVar y)
                               fvs      = fromList [x]
                                          `union` (fv n)
                                          `union` (fv (Abs y m))

-- Lexikografisch nächsten Variablennamen generieren
nextVar          :: String -> String
nextVar []       =  "a"
nextVar (x : xs) =  if x == 'z'
                      then 'a' : (nextVar xs)
                      else chr (ord x + 1) : xs


-- Auswertung
-- ==========

-- Gibt es Redex?
hasRedex                   :: LambdaTerm -> Bool
hasRedex (Var _)           =  False
hasRedex (App (Abs _ _) _) =  True
hasRedex (App t t')        =  hasRedex t || hasRedex t'
hasRedex (Abs _ t)         =  hasRedex t

-- Reduktion des linken Redex
normalOrderReduce                   :: LambdaTerm -> LambdaTerm
normalOrderReduce (Var t)           =  Var t
normalOrderReduce (App (Abs x t) n) =  subst n x t
normalOrderReduce (Abs x t)         =  Abs x (normalOrderReduce t)
normalOrderReduce (App t t')        =  if hasRedex t
                                        then App (normalOrderReduce t) t'
                                        else if hasRedex t'
                                          then App t (normalOrderReduce t')
                                          else App t t'

-- Normal order evaluation
normalOrderEval   :: LambdaTerm -> LambdaTerm
normalOrderEval t =  if hasRedex t
                       then normalOrderEval (normalOrderReduce t)
                       else t

-- Verbesserte normal order Auswertung: Redex suchen und reduzieren in einem Schritt
nOReduce                   :: LambdaTerm -> Maybe LambdaTerm
nOReduce (Var t)           =  Nothing
nOReduce (App (Abs x t) n) =  Just (subst n x t)
nOReduce (Abs x t)         =  if isNothing tr
                                then Nothing
                                else Just (Abs x (fromJust tr))
                              where tr = nOReduce t
nOReduce (App t t')        =  if isJust tr
                                then Just (App (fromJust tr) t')
                                else if isJust tr'
                                       then Just (App t (fromJust tr'))
                                       else Nothing
                              where tr  = nOReduce t
                                    tr' = nOReduce t'

nOEval :: LambdaTerm -> LambdaTerm
nOEval t = if isNothing tr
             then t
             else nOEval (fromJust tr)
           where tr = nOReduce t
