{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt06 where

type Matrix a = [Row a]
type Row a    = [a]

type Grid     = Matrix Digit
type Digit    = Char

toMatrix :: String -> Grid
toMatrix s 
  | (len /= sLen * sLen) = error ("Input needs to be of length n³, but has length " ++ show len)
  | otherwise = splitInto sLen s
  where sLen = sqrtLen s
        len = length s

sqrtLen :: [a] -> Int
sqrtLen = floor . (sqrt :: Double -> Double) . fromIntegral . length

{- 12
   21 -}
test1 :: Grid
test1 = toMatrix "1221"

{- 2341
   4132
   1423
   3214 -}
test2 :: Grid
test2 = toMatrix "2341413214233214"


{- 534678912
   672195348
   198342567
   859761423
   426853791
   713924856
   961537284 
   287419635
   345286179 -}
test3 :: Grid
test3 = toMatrix "534678912672195348198342567859761423426853791713924856961537284287419635345286179"

printGrid :: Grid -> IO ()
printGrid = putStr . buildS
  where buildS [] = ""
        buildS (x:xs) = x ++ "\n" ++ buildS xs


nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

valid :: Grid -> Bool
valid g = all nodups g          -- keine Duplikate in den Zeilen
       && all nodups (cols g)   -- keine Duplikate in den Spalten
       && all nodups (groups g) -- keine Duplikate in den Gruppen

cols :: Grid -> Grid
cols [] = []
cols [xs] = [[x] | x <- xs]
cols (x:xs) = zipWith (:) x (cols xs)

groups :: Grid -> Grid
groups g = zipN len . map (splitInto len) $ g
  where len = sqrtLen g

-- Splittet eine n² lange Zeile in n Teile mit Länge n
splitInto :: Int -> Row a -> [Row a]
splitInto _ [] = []
splitInto n xs = as : splitInto n bs
  where (as, bs) = splitAt n xs

-- Zippt n² Rows die in n Teile gesplittet wurden 
-- element weise zu n² Gruppe zusammen 
-- r1 :[["ab", "cd"],
-- r2 : ["ef", "gh"],  ==\    ["abef", "cdgh", "ijmn", "klop"]
-- r3 : ["ij", "kl"],  ==/     
-- r4 : ["mn", "op"]]
-- 34
zipN :: Int -> [Grid] -> Grid
zipN _ [] = []
zipN n xs = zip' (take n xs) ++ zipN n (drop n xs)
  where zip' [] = []
        zip' (y:ys) = foldl (zipWith (++)) y ys
