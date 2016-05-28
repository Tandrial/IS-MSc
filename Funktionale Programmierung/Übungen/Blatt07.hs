{-# OPTIONS_GHC -Wall -Werror #-}

module Blatt07 where
import Blatt06
import Blatt05
import Data.Char

type Choices = [Digit]

choices :: Grid -> Matrix Choices
choices g = map (map choice) g
  where choice d = if d == '0' then possible else [d]
        possible = ['1'..chr(ord '0' + length g)]

expand :: Matrix Choices-> [Grid]
expand = cp . map cp

solve :: Grid -> [Grid]
solve = filter (valid) . expand . choices

--------------------------------------------------------------------------------
-- TESTS EMPTY GRIDS                                                          --
--------------------------------------------------------------------------------
t2Empty :: Grid
t2Empty = toGrid "0000"

t4Empty :: Grid
t4Empty = toGrid "0000000000000000"

t9Empty :: Grid
t9Empty = toGrid "000000000000000000000000000000000000000000000000000000000000000000000000000000000"

--------------------------------------------------------------------------------
-- TESTS PARTLY FILLED  GRIDS                                                 --
--------------------------------------------------------------------------------
t2Part :: Grid
t2Part = toGrid "1021"

t4Part :: Grid
t4Part = toGrid "1234000000004321"

t9Part :: Grid
t9Part = toGrid "034600912672195300000000067859761400000000000013924800961500084207419000005280009"

