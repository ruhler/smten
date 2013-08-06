
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Sudoku.Solver where

import Smten.Prelude
import Smten.Symbolic
import Smten.Sudoku.Cell
import Smten.Sudoku.Board

solve :: (Eq c, Cell c) => Solver -> Symbolic (Board c) -> IO (Maybe (Board c))
solve s board = run_symbolic s $ do
    b <- board
    assert (isvalid b)
    return b
    
