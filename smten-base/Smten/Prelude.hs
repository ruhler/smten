
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Prelude (
    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, IO,

    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(..),
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Monad(..),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_,
    maybe, either,
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
    FilePath, putChar, putStr, putStrLn, getContents,

    ifThenElse, primitive,
    ) where

import Smten.Control.Monad
import Smten.Data.Bool
import Smten.Data.Function
import Smten.Data.Functor
import Smten.Data.Either
import Smten.Data.Enum
import Smten.Data.EnumInteger ()
import Smten.Data.EnumInt ()
import Smten.Data.Eq
import Smten.Data.Ord
import Smten.Data.List
import Smten.Data.Maybe
import Smten.Data.Num
import Smten.Data.Show
import Smten.Data.Tuple
import Smten.Smten.Base
import Smten.System.IO

undefined :: a
undefined = error "Prelude.undefined"

{-# NOINLINE primitive #-}
primitive :: String -> a
primitive nm = error ("Smten.primitive: " ++ nm)

