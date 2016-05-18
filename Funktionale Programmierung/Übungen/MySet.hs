module MySet (intersect) where

-- Realisieren Sie hier die Funktion intersect.
-- Zur Anwendung des Moduls muss sich die Datei MySet.hs im gleichen
-- Verzeichnis befinden wie das Skript, das das Modul importiert.

-- Aufgabe 6.1

intersect :: Eq a => [a] -> [a] -> [a]
intersect p q = [x | x <- p, y <- q, x == y]

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' p q = foldl (\xs y -> if elem y p then y:xs else xs) [] q
