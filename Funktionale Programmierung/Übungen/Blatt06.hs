{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt06 where

type Matrix a = [Row a]
type Row a    = [a]

type Grid     = Matrix Digit
type Digit    = Char

toMatrix :: String -> Grid
toMatrix s 
  | (len /= sLen * sLen) = error ("Input needs to be of length n³, but has length " ++ show len)
  | otherwise            = splitEach sLen s
  where sLen = sqrtLen s
        len  = length s

sqrtLen :: [a] -> Int
sqrtLen = floor . (sqrt :: Double -> Double) . fromIntegral . length

--------------------------------------------------------------------------------
-- VALID TESTS
--------------------------------------------------------------------------------
test1 :: Grid
test1 = toMatrix "1221"

test2 :: Grid
test2 = toMatrix "2341413214233214"

test3 :: Grid
test3 = toMatrix "534678912672195348198342567859761423426853791713924856961537284287419635345286179"

--------------------------------------------------------------------------------
-- INVALID TESTS
--------------------------------------------------------------------------------
test4 :: Grid -- 1 Fehler in Zeile
test4 = toMatrix "1213342521344351"

test5 :: Grid -- 1 Fehler in Spalte
test5 = toMatrix "1243341287413569"

test6 :: Grid -- 1 Fehler in Gruppe
test6 = toMatrix "1243312525344351"

printGrid :: Grid -> IO ()
printGrid = putStr . buildS
  where buildS []     = ""
        buildS (x:xs) = x ++ "\n" ++ buildS xs


nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = all (/= x) xs && nodups xs

valid :: Grid -> Bool
valid g = all nodups g          -- Zeilen
       && all nodups (cols g)   -- Spalten
       && all nodups (groups g) -- Gruppen

-- Transponiert ein  Grid
cols :: Grid -> Grid
cols []     = []
cols [xs]   = [[x] | x <- xs]
cols (x:xs) = zipWith (:) x (cols xs)

-- Grid mit [r1, r2, r3, r4] => Grid mit [g1, g2, g3, g4]
-- ["abcd",
--  "efgh",  ==\     ["abef", "cdgh", "ijmn", "klop"]
--  "ijkl",  ==/
--  "mnop"]
groups :: Grid -> Grid
groups g = zipN len . map (splitEach len) $ g
  where len = sqrtLen g

-- Splittet eine n² lange Zeile in n Teile mit Länge n
-- ["abcd"]      ==> ["ab", "cd"]
splitEach :: Int -> Row a -> [[a]]
splitEach _ [] = []
splitEach n xs = as : splitEach n bs
  where (as, bs) = splitAt n xs

-- Zippt n² Rows die in n Teile gesplittet wurden 
-- element weise zu n² Gruppe zusammen 
-- [["ab", "cd"],
--  ["ef", "gh"],  ==\    ["abef", "cdgh", "ijmn", "klop"]
--  ["ij", "kl"],  ==/     
--  ["mn", "op"]]
zipN :: Int -> [Grid] -> Grid
zipN _ [] = []
zipN n xs = zip' (take n xs) ++ zipN n (drop n xs)
  where zip' []     = []
        zip' (y:ys) = foldl (zipWith (++)) y ys
