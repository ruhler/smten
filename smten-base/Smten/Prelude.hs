
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
    Integral(..), Real(..),
    Monad(..),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_,
    maybe, either,
    (&&), (||), not, otherwise,
    fst, snd, curry, uncurry,
    id, const, (.), flip, ($),
    error, undefined,
    seq, ($!),

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
    ReadS, Read(..), reads, read,

    -- PreludeIO
    FilePath, putChar, putStr, putStrLn, print, getContents,

    primitive,
    ) where

import Prelude hiding (
  undefined,
  ReadS, Read(..), reads, read,
 )

-- TODO: Switch to GHC Prelude for these things instead of redefining them.
import Smten.Data.Read

undefined :: a
undefined = error "Prelude.undefined"

{-# NOINLINE primitive #-}
primitive :: String -> a
primitive nm = error ("Smten.primitive: " ++ nm)

