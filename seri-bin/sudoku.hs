
import Prelude hiding (notElem, all, print)

import Data.Functor ((<$>))
import Data.Maybe

import Seri.Type
import Seri.ExpH
import Seri.SMT.Query
import Seri.HaskellF.Symbolic
import Seri.HaskellF.Query
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified Seri.HaskellF.Lib.SMT as S
import qualified Seri_SMT as S
import Seri.SMT.Specialize
import Seri.SMT.Yices.Yices2    

import Data.List(transpose)

-- The size of the sudoku.
-- (nxn)x(nxn)
n :: Integer
n = 3

-- The square of the sudoku size.
m :: Integer
m = n*n

type Cell = Integer
type S_Cell = S.Integer

freeCell :: Query S_Cell
freeCell = do
    x <- freeS
    assertS ((x S.> 0) S.&& (x S.<= seriS m))
    return x

readCell :: Char -> Query S_Cell;
readCell '1' = return 1;
readCell '2' = return 2;
readCell '3' = return 3;
readCell '4' = return 4;
readCell '5' = return 5;
readCell '6' = return 6;
readCell '7' = return 7;
readCell '8' = return 8;
readCell '9' = return 9;
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

notElem :: (S.Eq a, Symbolic a) => a -> [a] -> S.Bool
notElem x [] = seriS True
notElem x (y:ys) = (x S./= y) S.&& (notElem x ys)

-- Return true if all elements in the list are unique.
unique :: (Symbolic a, S.Eq a) => [a] -> S.Bool;
unique [] = seriS True;
unique (x:xs) = notElem x xs S.&& unique xs;

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

all :: (a -> S.Bool) -> [a] -> S.Bool
all f [] = seriS True
all f (x:xs) = S.__caseTrue (f x) (all f xs) (seriS False)

isvalid :: [[S_Cell]] -> S.Bool;
isvalid b = all unique (concat [rows b, cols b, boxes b]);

readRow :: [Char] -> Query [S_Cell];
readRow = mapM readCell;

readBoard :: [[Char]] -> Query [[S_Cell]]
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

solve :: Query [[Char]];
solve = do
    board <- readBoard solved
    assert (unbox $ isvalid board)
    result <- query $ mapM (mapM realizeS) board
    case (result) of
       Satisfiable v -> return (print v)
       _ -> return ["no solution"]

main :: IO ()
main = do
    y <- yices2
    let l = core { th_integer = True, th_bit = True }
    r <- runQuery (RunOptions (Just "build/test/sudoku.dbg") y l) solve
    mapM_ putStrLn r

