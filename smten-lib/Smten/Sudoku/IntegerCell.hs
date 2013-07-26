
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Sudoku.IntegerCell where

import Smten.Prelude
import Smten.Data.Function
import Smten.Symbolic
import Smten.Sudoku.Cell

data IntegerCell = IntegerCell { icv :: Integer }

instance Eq IntegerCell where
    (==) = (==) `on` icv

instance Cell IntegerCell where
    mkCell = IntegerCell

    deCell (IntegerCell x) = x

    freeCell = do
        x <- free_Integer
        assert ((x > 0) && (x <= 9))
        return (IntegerCell x)

    distinctCell = distinct



