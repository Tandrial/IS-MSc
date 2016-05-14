module MySet (intersect) where

-- Realisieren Sie hier die Funktion intersect.
-- Zur Anwendung des Moduls muss sich die Datei MySet.hs im gleichen
-- Verzeichnis befinden wie das Skript, das das Modul importiert.

intersect :: [String] -> [String] -> [String]
intersect p q = [x | x <- p, y <- q, x == y]

intersect' :: [String] -> [String] -> [String]
intersect' p q = foldl (\xs y -> if elem y p then y:xs else xs) [] q
