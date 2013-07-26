
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Sudoku.BitCell where

import Smten.Prelude
import Smten.Symbolic
import Smten.Data.Bit
import Smten.Data.Function

import Smten.Sudoku.Cell

data BitCell = BitCell { bits :: Bit 9 }

instance Eq BitCell where
    (==) = (==) `on` bits

instance Show BitCell where
    show = show . bits

c1 :: BitCell
c1 = BitCell 1

c2 :: BitCell
c2 = BitCell 2

c3 :: BitCell
c3 = BitCell 4

c4 :: BitCell
c4 = BitCell 8

c5 :: BitCell
c5 = BitCell 16

c6 :: BitCell
c6 = BitCell 32

c7 :: BitCell
c7 = BitCell 64

c8 :: BitCell
c8 = BitCell 128

c9 :: BitCell
c9 = BitCell 256

isValidCell :: BitCell -> Bool
isValidCell c = elem c [c1, c2, c3, c4, c5, c6, c7, c8, c9]

join :: [BitCell] -> Bit 9
join xs = foldl bv_or 0 (map bits xs)

instance Cell BitCell where
    mkCell i = [c1, c2, c3, c4, c5, c6, c7, c8, c9] !! (fromInteger $ i-1)

    deCell c = 
           if c == c1 then 1
      else if c == c2 then 2
      else if c == c3 then 3
      else if c == c4 then 4
      else if c == c5 then 5
      else if c == c6 then 6
      else if c == c7 then 7
      else if c == c8 then 8
      else 9

    freeCell = do
        x <- free_Bit
        let b = BitCell x
        assert (isValidCell b)
        return b

    distinctCell cells = join cells == 0x1FF

