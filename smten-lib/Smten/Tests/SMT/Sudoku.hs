
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Sudoku where

import Smten.Prelude
import Smten.Symbolic
import Smten.Sudoku.Cell
import Smten.Sudoku.Board
import Smten.Sudoku.Boards
import Smten.Sudoku.Solver
import Smten.Sudoku.IntegerCell
import Smten.Sudoku.EnumCell
import Smten.Sudoku.BitCell
import Smten.Tests.Test

sudokutest :: forall c . (Cell c, Eq c) => Solver -> String -> c -> IO ()
sudokutest slv msg _ = do
    putStrLn $ msg ++ "..."
    board <- solve slv (readBoard diabolical :: Symbolic (Board c))
    let wnt = Just diabolical_solved :: Maybe [String]
        got = fmap printBoard board :: Maybe [String]
    test msg (wnt == got)

tests :: Solver -> IO ()
tests slv = do
    sudokutest slv "Sudoku.Integer" (undefined :: IntegerCell)
    sudokutest slv "Sudoku.Enum" (undefined :: EnumCell)
    sudokutest slv "Sudoku.Bit" (undefined :: BitCell)
    putStrLn "Sudoku PASSED"

