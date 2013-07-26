
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Sudoku.Cell where

import Smten.Prelude
import Smten.Data.Maybe
import Smten.Symbolic

-- The size of the sudoku.
-- (nxn)x(nxn)
n :: Int
n = 3

-- The square of the sudoku size.
m :: Int
m = n*n

-- Return true if all elements in the list are distinct.
distinct :: (Eq a) => [a] -> Bool
distinct [] = True
distinct (x:xs) = notElem x xs && distinct xs

class Cell c where
    mkCell :: Integer -> c
    deCell :: c -> Integer
    freeCell :: Symbolic c
    distinctCell :: [c] -> Bool
    
readCell :: (Cell c) => Char -> Symbolic c
readCell c = fromMaybe (error ("readCell: " ++ [c])) (lookup c [
    ('1', return (mkCell 1)),
    ('2', return (mkCell 2)),
    ('3', return (mkCell 3)),
    ('4', return (mkCell 4)),
    ('5', return (mkCell 5)),
    ('6', return (mkCell 6)),
    ('7', return (mkCell 7)),
    ('8', return (mkCell 8)),
    ('9', return (mkCell 9)),
    ('.', freeCell)])

printCell :: (Cell c) => c -> Char
printCell c = fromMaybe '?' (lookup (deCell c) [
    (1, '1'), (2, '2'), (3, '3'), (4, '4'),
    (5, '5'), (6, '6'), (7, '7'), (8, '8'),
    (9, '9')])

