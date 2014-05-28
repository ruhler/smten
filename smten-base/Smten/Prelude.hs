
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

-- These are imported to ensure they are compiled.
import Smten.Base.Control.Monad ()
import Smten.Base.Data.Functor ()
import Smten.Base.Data.Either ()
import Smten.Base.Data.List ()
import Smten.Base.Data.Maybe ()
import Smten.Base.Data.Tuple ()
import Smten.Base.GHC.Base ()
import Smten.Base.GHC.Char ()
import Smten.Base.GHC.Enum ()
import Smten.Base.GHC.List ()
import Smten.Base.GHC.Num ()
import Smten.Base.GHC.Real ()
import Smten.Base.GHC.Show ()

import Smten.System.IO_ ()
import Smten.Smten.Char ()
import Smten.Smten.Int ()

-- TODO: Switch to GHC Prelude for these things instead of redefining them.
import Smten.Data.Read

undefined :: a
undefined = error "Prelude.undefined"

{-# NOINLINE primitive #-}
primitive :: String -> a
primitive nm = error ("Smten.primitive: " ++ nm)

