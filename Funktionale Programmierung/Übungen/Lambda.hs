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


-- Beispielterme
-- =============

a = "a"; va = Var a
b = "b"; vb = Var b
f = "f"; vf = Var f
g = "g"; vg = Var g
l = "l"; vl = Var l
m = "m"; vm = Var m
n = "n"; vn = Var n
p = "p"; vp = Var p
s = "s"; vs = Var s
x = "x"; vx = Var x
y = "y"; vy = Var y
z = "z"; vz = Var z

-- xx
t1 = App vx vx

-- y\x.y
t2 = App vy (Abs x vy)

-- \y.yx
t3 = Abs y (App vy vx)

-- \xy.yx
t4 = Abs x (Abs y (App vy vx))

-- \y.y(\x.x)x
t5 = Abs y (App (App vy (Abs x vx)) vx)

-- \xy.y(\x.x)x
t6 = Abs x t5

-- \x.xx
t7 = Abs x (App vx vx)

-- (\x.y)((\x.xx)(\x.xx))
t8 = App (Abs x vy) (App t7 t7)

-- (\x.x((\y.xy)x))(\z.zy)
t9 = App (Abs x (App vx (App (Abs y (App vx vy)) vx))) (Abs z (App vz vy))


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


-- Daten und wichtige Funktionen im Lambda-Kalkül
-- ==============================================

-- Boolesche Werte und Funktionen
hT   = \x y -> x
hF   = \x y -> y

-- Abbildung von hT und hF auf True und False, um Wahrheitswerte
-- in Session darstellen zu können
isTrue b = b True False

hNot = \x   -> x hF hT
hAnd = \x y -> x y hF
hOr  = \x y -> x hT y

lT   = Abs x (Abs y vx)   -- \xy.x
lF   = Abs x (Abs y vy)   -- \xy.y

lNot = Abs x (App (App vx lF) lT)           -- \x.xFT
lAnd = Abs x (Abs y (App (App vx vy) lF))   -- \xy.xyF
lOr  = Abs x (Abs y (App (App vx lT) vy))   -- \xy.xTy


-- 2-Tupel

hPair = \a b p -> p a b
hFst  = \p     -> p (\a b -> a)
hSnd  = \p     -> p (\a b -> b)

lPair = Abs a (Abs b (Abs p (App (App vp va) vb)))
lFst  = Abs p (App vp (Abs a (Abs b va)))
lSnd  = Abs p (App vp (Abs a (Abs b vb)))

{-
hPair x y
= (\a b p -> p a b) x y
= \p -> p x y

hFst (hPair x y)
= hFst (\p -> p x y)
= (\p -> p (\a b -> a)) (\p -> p x y)
= (\p -> p x y) (\a b -> a)
= (\a b -> a) x y
= (\b -> x) y
= x
-}


-- Listen

hHead  = hFst
hTail  = hSnd
hIsEmpty = \l -> l (\a b -> hF)
hNil   = \x -> hT

lHead  = lFst
lTail  = lSnd
lIsEmpty = Abs l (App vl (Abs a (Abs b lF)))
lNil   = Abs x lT

{-
hIsEmpty (hPair x y)
= hIsEmpty (\p -> p x y)
= (\l -> l (\a b -> hF)) (\p -> p x y)
= (\p -> p x y) (\a b -> hF)
= (\a b -> hF) x y
= hF

hIsEmpty hNil
= (\l -> l (\a b -> hF)) (\x -> hT)
= (\x -> hT) (\a b -> hF)
= hT
-}


-- Natürliche Zahlen

h0 = \s z -> z
h1 = \s z -> s z
h2 = \s z -> s (s z)
h3 = \s z -> s (s (s z))
h4 = \s z -> s (s (s (s z)))

int2hChurch 0       = \s z -> z
int2hChurch (n + 1) = \s z -> s (int2hChurch n s z)

hChurch2int t = t (1 +) 0

l0 = int2lChurch 0
l1 = int2lChurch 1
l2 = int2lChurch 2
l3 = int2lChurch 3
l4 = int2lChurch 4

int2lChurch 0       = Abs s (Abs z vz)
int2lChurch (n + 1) = Abs s (Abs z (normalOrderEval (App vs (App (App (int2lChurch n) vs) vz))))

lChurch2int (Abs s (Abs z t)) = lChurch2int' t
                                where lChurch2int' (Var _)         = 0
                                      lChurch2int' (App (Var _) t) = 1 + lChurch2int' t

hIsZero = \n       -> n (\x -> hF) hT
hSucc   = \n s z   -> s (n s z)
hAdd    = \a b s z -> a s (b s z)
hMult   = \a b s   -> a (b s)
hExp    = \a b     -> b a

hSucc' = \p -> hPair (hSucc (hFst p)) (hFst p)
hPred  = \n -> hSnd (n hSucc' (hPair h0 h0))

lIsZero = Abs n (App (App vn (Abs x lF)) lT)
lSucc   = Abs n (Abs s (Abs z (App vs (App (App vn vs) vz))))
lAdd    = Abs a (Abs b (Abs s (Abs z (App (App va vs) (App (App vb vs) vz)))))
lMult   = Abs a (Abs b (Abs s (App va (App vb vs))))
lExp    = Abs a (Abs b (App vb va))

{-
hMult |0| |n|
= (\a b s -> a (b s)) (\s z -> z) |n|
= (\b s -> (\s z -> z) (b s)) |n|
= \s -> (\s z . z) (|n| s)
= \s -> (\z . z)
= \s z -> z
=  |0|

hMult |m + 1| |n|
= (\a b s -> a (b s)) |m + 1| |n|
= (\b s -> |m + 1| (b s)) |n|
= \s -> |m + 1| (|n| s)
= \s -> (\s z -> s (|m| s z)) (|n| s)
= \s -> (\z -> (|n| s) (|m| (|n| s) z))
= \s z -> (|n| s) (|m| (|n| s) z)
= \s z -> (|n| s) (mult |m| |n| s z)
= hAdd |n| (hMult |m| |n|)
-}

lSucc' = Abs p (App (App lPair (App lSucc (App lFst vp))) (App lFst vp))
lPred  = Abs n (App lSnd (App (App vn lSucc') (App (App lPair l0) l0)))


-- Rekursion

-- FP = (\xf.f(xxf)) (\xf.f(xxf))
lFP = App (Abs x (Abs f (App vf (App (App vx vx) vf)))) (Abs x (Abs f (App vf (App (App vx vx) vf))))

-- fak' = \fn.(isZero n) 1 (mult n (f (pred n)))
lFak' = Abs f (Abs n (App (App (App lIsZero vn) l1) (App (App lMult vn) (App vf (App lPred vn)))))

-- fak = FP fak'
lFak = App lFP lFak'
