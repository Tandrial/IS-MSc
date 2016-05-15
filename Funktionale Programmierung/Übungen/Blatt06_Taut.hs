{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt06_Taut where

import MySet (intersect)

-- Datentyp zur Repräsentation von Propositionen
data Prop = Atom String
          | Neg Prop
          | Conj Prop Prop
          | Disj Prop Prop
          deriving Show

-- Definitionen zur Bildung beispielhafter Propositionen

implies :: (Prop, Prop) -> Prop
implies (p, q) = Disj (Neg p) q

p1 ::Prop
p1 = Atom "a"

p2 ::Prop
p2 = Atom "b"

p3 ::Prop
p3 = Atom "c"

assumption1 ::Prop
assumption1 = implies (p1, p2)

assumption2 ::Prop
assumption2 = Neg (Conj p2 p3)

conclusion ::Prop
conclusion = implies (p1, Neg p3)

goal ::Prop
goal = implies (Conj assumption1 assumption2, conclusion)

-- String-Darstellungen von Propositionen

-- vollständig geklammert
show1 :: Prop -> String
show1 (Atom a) = a
show1 (Neg p) = "~" ++ (show1 p)
show1 (Conj p q) = "(" ++ show1 p ++ " & " ++ show1 q ++ ")"
show1 (Disj p q) = "(" ++ show1 p ++ " | " ++ show1 q ++ ")"

-- Aufgabe 6.1

-- weniger Klammern aufgrund von Assoziativität und Bindungsstärke
show2 :: Prop -> String
show2 (Atom a) = a
show2 (Neg p@(Atom _)) = "~" ++ show2 p
show2 (Neg p) = "~(" ++ show2 p ++ ")"
show2 (Conj p@(Disj _ _) q@(Disj _ _)) = "(" ++ show2 p ++ ") & (" ++ show2 q ++ ")"
show2 (Conj p@(Disj _ _) q) = "(" ++ show2 p ++ ") & " ++ show2 q
show2 (Conj p q@(Disj _ _)) = show2 p ++ " & (" ++ show2 q ++ ")"
show2 (Conj p q) = show2 p ++ " & " ++ show2 q
show2 (Disj p q) = show2 p ++ " | " ++ show2 q

-- Funktionen zur Lösung des Tautologie-Problems

-- Ziel ist es, Terme in konjunktive Normalform zu überführen, also in
-- Konjunktion von Disjunktionen, die nur Atome oder negierte Atome enthalten.
-- Wenn in jeder Disjunktion ein Atom sowohl negiert also auch unnegiert
-- auftritt, ist der Ausdruck eine Tautologie.

-- negative-normal-form:
-- nur Atome treten negiert auf; Negationen wandern ins Innere der Terme
nnf :: Prop -> Prop
nnf (Atom p)         = Atom p
nnf (Neg (Atom p))   = Neg (Atom p)
nnf (Neg (Neg p))    = nnf p
nnf (Neg (Conj p q)) = nnf (Disj (Neg p) (Neg q))
nnf (Neg (Disj p q)) = nnf (Conj (Neg p) (Neg q))
nnf (Conj p q)       = Conj (nnf p) (nnf q)
nnf (Disj p q)       = Disj (nnf p) (nnf q)

-- überführt Proposition von negative-normal-form
-- in konjunktive Normalform
cnf :: Prop -> Prop
cnf (Conj p q) = Conj (cnf p) (cnf q)
cnf (Disj p q) = distrib (cnf p) (cnf q)
cnf p          = p

-- verschiebt Disjunktion durch Anwendung des
-- Distributivgesetztes nach innen
distrib :: Prop -> Prop -> Prop
distrib p (Conj q r) = Conj (distrib p q) (distrib p r)
distrib (Conj q r) p = Conj (distrib q p) (distrib r p)
distrib p q          = Disj p q

-- bestimmt Menge der positiven Atome einer Disjunktion
positives :: Prop -> [String]
positives (Atom a)       = [a]
positives (Neg (Atom _)) = []
positives (Disj p q)     = positives p ++ positives q
positives _              = error "Only works with the output of cnf"

-- bestimmt Menge der negativen Atome einer Diskunktion
negatives :: Prop -> [String]
negatives (Atom _)       = []
negatives (Neg (Atom a)) = [a]
negatives (Disj p q)     = negatives p ++ negatives q
negatives _              = error "Only works with the output of cnf"

taut :: Prop -> Bool
taut =  taut' . cnf . nnf
  where taut' (Conj p q) = taut' p && taut' q
        taut' p          = (not . null) (intersect (positives p) (negatives p))
