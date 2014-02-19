
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Integral0 (
    int_quot, int_rem,
 )  where

import qualified Prelude as P

import Smten.Runtime.SymbolicOf
import Smten.Compiled.Smten.Smten.Int
import Smten.Compiled.Smten.Smten.Integer

int_quot :: Int -> Int -> Int
int_quot = {-# SCC "PRIM_INT_QUOT" #-} symapp2 (\x y -> tosym (P.quot (x :: P.Int) y))

int_rem :: Int -> Int -> Int
int_rem = {-# SCC "PRIM_INT_REM" #-} symapp2 (\x y -> tosym (P.rem (x :: P.Int) y))

