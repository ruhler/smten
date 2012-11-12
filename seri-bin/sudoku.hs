
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding ((>), (==), (/=), (<=), notElem, print, (&&), all)

import Data.List(transpose)

import Seri
import Seri.Failable
import Seri.Lambda hiding (free, query)
import Seri.DSEL.DSEL
import Seri.DSEL.SMT
import Seri.Type.SeriT
import Seri.TH
import Seri
import Seri.SMT.Solver (Solver)
import Seri.SMT.Yices.Yices2


-- The size of the sudoku.
-- (nxn)x(nxn)
n :: Integer
n = 3

-- The square of the sudoku size.
m :: Integer
m = n*n

type Cell = Integer

freeCell :: (Solver s) => Query s (ExpT Cell)
freeCell = do
    x <- free
    assert ((x > 0) && (x <= seriET m))
    return x

readCell :: (Solver s) => Char -> Query s (ExpT Cell);
readCell '1' = return (seriET 1);
readCell '2' = return (seriET 2);
readCell '3' = return (seriET 3);
readCell '4' = return (seriET 4);
readCell '5' = return (seriET 5);
readCell '6' = return (seriET 6);
readCell '7' = return (seriET 7);
readCell '8' = return (seriET 8);
readCell '9' = return (seriET 9);
readCell '.' = freeCell;
readCell c = error ("readCell: " ++ [c]);

printCell :: Cell -> Char;
printCell 1 = '1';
printCell 2 = '2';
printCell 3 = '3';
printCell 4 = '4';
printCell 5 = '5';
printCell 6 = '6';
printCell 7 = '7';
printCell 8 = '8';
printCell 9 = '9';

notElem :: (SeriT a) => ExpT a -> [ExpT a] -> ExpT Bool
notElem x [] = seriET True
notElem x (y:ys) = (x /= y) && (notElem x ys)

-- Return true if all elements in the list are unique.
unique :: (SeriT a) => [ExpT a] -> ExpT Bool;
unique [] = seriET True;
unique (x:xs) = notElem x xs && unique xs;

print :: [[Cell]] -> [[Char]];
print cells = map (\row -> map printCell row) cells;

rows :: [[a]] -> [[a]];
rows x = x;

cols :: [[a]] -> [[a]];
cols x = transpose x;

boxes :: [[a]] -> [[a]];
boxes rows = 
  let {
    brows = breakup n rows;
  } in concat (map boxes' brows);

-- Given just 'm' rows, return the m boxes in those m rows.
boxes' :: [[a]] -> [[a]];
boxes' [] = [];
boxes' ([]:_) = [];
boxes' xs = 
  let {
    b = concat (map (take (fromInteger n)) xs);
    bs = map (drop (fromInteger n)) xs;
  } in b : (boxes' bs);

-- Break a list up into a bunch of lists of the given length.
breakup :: Integer -> [a] -> [[a]];
breakup _ [] = [];
breakup n xs =
  case splitAt (fromInteger n) xs of {
     (a, b) -> a : (breakup n b);
  };

all :: (a -> ExpT Bool) -> [a] -> ExpT Bool
all f [] = seriET True
all f (x:xs) = ite (f x) (all f xs) (seriET False)

isvalid :: [[ExpT Cell]] -> ExpT Bool;
isvalid b = all unique (concat [rows b, cols b, boxes b]);

readRow :: (Solver s) => [Char] -> Query s [ExpT Cell];
readRow = mapM readCell;

readBoard :: (Solver s) => [[Char]] -> Query s [[ExpT Cell]]
readBoard rows = mapM readRow rows;

easy :: [[Char]];
easy =
  ["2....1.38",
   "........5",
   ".7...6...",
   ".......13",
   ".981..257",
   "31....8..",
   "9..8...2.",
   ".5..69784",
   "4..25...."];

solved :: [[Char]];
solved =
  ["249571638",
   "861432975",
   "573986142",
   "725698413",
   "698143257",
   "314725869",
   "937814526",
   "152369784",
   "486257391"];

diabolical :: [[Char]];
diabolical = 
    [".9.7..86.",
     ".31..5.2.",
     "8.6......",
     "..7.5...6",
     "...3.7...",
     "5...1.7..",
     "......1.9",
     ".2.6..35.",
     ".54..8.7."];

solve :: (Solver s) => Query s [[Char]];
solve = do
    board <- readBoard diabolical
    assert (isvalid board)
    result <- queryR $ mapM (mapM realize) board
    case (result) of
       Satisfiable v -> return (print v)
       _ -> return ["no solution"]

env :: Env
env = $(loadenvth [seridir] (seridir >>= return . (++ "/Seri/SMT/SMT.sri")))

main :: IO ()
main = runQuery (RunOptions (Just "build/test/sudoku.dbg") True) env (yices2 solve) >>= mapM_ putStrLn

