
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Prelude (
    Bool(False, True),
    Maybe(Nothing, Just),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, IO,

    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(..),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Monad(..),
    mapM, mapM_, sequence, sequence_,
    maybe,
    (&&), (||), not, otherwise,
    fst, snd, curry, uncurry, id, const, (.), flip, ($),
    error, undefined,

    -- PreludeList
    map, (++), filter, concat, concatMap,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

    -- PreludeText
    ShowS, Show(..), shows, showChar, showString, showParen,

    -- PreludeIO
    putChar, putStr, putStrLn,

    ifThenElse,
    ) where

import Smten.Control.Monad
import Smten.Data.Bool
import Smten.Data.Function
import Smten.Data.Enum
import Smten.Data.EnumInteger ()
import Smten.Data.Eq
import Smten.Data.Ord
import Smten.Data.List
import Smten.Data.Maybe
import Smten.Data.Num
import Smten.Data.Show
import Smten.Data.Tuple
import Smten.Smten.Base
import Smten.System.IO

