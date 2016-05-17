{-# OPTIONS_GHC -Wall -Werror #-}
module Blatt06 where

type Matrix a = [Row a]
type Row a    = [a]

type Grid     = Matrix Digit
type Digit    = Char

-- Aufgabe 6.2

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = all (/= x) xs && nodups xs

valid :: Eq a => Matrix a -> Bool
valid = foldl (&&) True . map (all nodups) . rmap [id, cols, groups]
  where rmap fs x = map ($x) fs

valid' :: Eq a => Matrix a -> Bool
valid' g = all nodups g          -- Zeilen
        && all nodups (cols g)   -- Spalten
        && all nodups (groups g) -- Gruppen

-- Transponiert ein Grid
cols :: Matrix a -> Matrix a
cols []     = []
cols [xs]   = [[x] | x <- xs]
cols (x:xs) = zipWith (:) x (cols xs)

-- Grid mit [r1, r2, r3, r4] => Grid mit [g1, g2, g3, g4]
-- ["abcd",
--  "efgh",  ==\     ["abef", "cdgh", "ijmn", "klop"]
--  "ijkl",  ==/
--  "mnop"]
groups :: Matrix a -> Matrix a
groups g = zipN len . map (splitEach len) $ g
  where len = sqrtLen g

sqrtLen :: [a] -> Int
sqrtLen = floor . (sqrt :: Double -> Double) . fromIntegral . length

-- Splittet eine n² lange Zeile in n Teile mit Länge n
-- ["abcd"]      ==> ["ab", "cd"]
splitEach :: Int -> Row a -> [Row a]
splitEach _ [] = []
splitEach n xs = as : splitEach n bs
  where (as, bs) = splitAt n xs

-- Zippt n² Rows die in n Teile gesplittet wurden
-- element weise zu n² Gruppe zusammen
-- [["ab", "cd"],
--  ["ef", "gh"],  ==\    ["abef", "cdgh", "ijmn", "klop"]
--  ["ij", "kl"],  ==/
--  ["mn", "op"]]
zipN :: Int -> [Matrix a] -> Matrix a
zipN _ [] = []
zipN n xs = zip' (take n xs) ++ zipN n (drop n xs)
  where zip' []     = []
        zip' (y:ys) = foldl (zipWith (++)) y ys

--------------------------------------------------------------------------------
--                                                                            --
--------------------------------------------------------------------------------

toGrid :: String -> Grid
toGrid s
  | len == (sLen * sLen) = splitEach sLen s
  | otherwise            = error ("Input needs to be of length n³, but has length " ++ show len)
  where sLen = sqrtLen s
        len  = length s

--------------------------------------------------------------------------------
-- VALID TESTS                                                                --
--------------------------------------------------------------------------------
t2 :: Grid
t2 = toGrid "1221"

t4 :: Grid
t4 = toGrid "2341413214233214"

t9 :: Grid
t9 = toGrid "534678912672195348198342567859761423426853791713924856961537284287419635345286179"

t16 :: Grid
t16 = toGrid ("DE20F8536BC9A471BFAC9461708D5E323659EB7AF412D08C78142C0D53EAFB96"
           ++ "1BC25E389FA07D64E3976A40DCB1852FAD687F193254EC0BF045CDB2E67813A9"
           ++ "C23AB7DE49056F1884BF1526CAD397E059E130AF876B42CD07D689C421FEBA53"
           ++ "217BA3850D46C9FE4A8ED29C153F06B79CFD06EBA8273145650341F7BE9C28DA")

--------------------------------------------------------------------------------
-- INVALID TESTS                                                              --
--------------------------------------------------------------------------------
tZ :: Grid -- 1 Fehler in Zeile
tZ = toGrid "1213342521344351"

tS :: Grid -- 1 Fehler in Spalte
tS = toGrid "1243341287413569"

tG :: Grid -- 1 Fehler in Gruppe
tG = toGrid "1243312525344351"

--------------------------------------------------------------------------------
-- Pretty Print a Sudoku Grid                                                 --
--------------------------------------------------------------------------------
printGrid :: Grid -> IO ()
printGrid g = putStr $ build "" '\n' (\x -> build "\n" ' ' (\y -> [y]) x) g
  where len                       = sqrtLen g
        build endStr bChar fun ys = build' len ys
          where build' _ []   = endStr
                build' n (x:xs)
                  | n == 0    = bChar : build' len (x:xs)
                  | otherwise = fun x ++ build' (n - 1) xs

pGrid :: Grid -> IO ()
pGrid g = putStr $ buildS len g
  where len           = sqrtLen g
        buildS _ []   = ""
        buildS n (x:xs)
          | n == 0    = '\n' : buildS len (x:xs)
          | otherwise = buildR len x ++ buildS (n - 1) xs
        buildR _ []   = "\n"
        buildR n (x:xs)
          | n == 0    = ' ' : buildR len (x:xs)
          | otherwise = [x] ++ buildR (n - 1) xs
