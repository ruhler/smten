
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Sudoku.EnumCell where

import Smten.Prelude
import Smten.Control.Monad

import Smten.Sudoku.Cell

data EnumCell = C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9

instance Eq EnumCell where
    (==) C1 C1 = True
    (==) C2 C2 = True
    (==) C3 C3 = True
    (==) C4 C4 = True
    (==) C5 C5 = True
    (==) C6 C6 = True
    (==) C7 C7 = True
    (==) C8 C8 = True
    (==) C9 C9 = True
    (==) _ _ = False

instance Cell EnumCell where
    mkCell i = [C1, C2, C3, C4, C5, C6, C7, C8, C9] !! (fromInteger $ i-1)

    deCell C1 = 1
    deCell C2 = 2
    deCell C3 = 3
    deCell C4 = 4
    deCell C5 = 5
    deCell C6 = 6
    deCell C7 = 7
    deCell C8 = 8
    deCell C9 = 9

    freeCell = msum (map return [C1, C2, C3, C4, C5, C6, C7, C8, C9])

    distinctCell = distinct

