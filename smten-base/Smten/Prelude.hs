
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
    Integral(quot, rem, quotRem, toInteger),
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
    FilePath, putChar, putStr, putStrLn, getContents,

    primitive,
    ) where

import Prelude (Bool(..), (&&), (||), not, otherwise)
import Prelude (Maybe(..), maybe)
import Prelude (Either(..), either)
import Prelude (id, const, (.), flip, ($))
import Prelude (Eq(..), Ordering(..), Ord(..))
import Prelude (Num(..))
import Prelude (Show(..), ShowS, shows, showChar, showString, showParen)
import Prelude (fst, snd, curry, uncurry)

-- Make sure these get compiled...
import Smten.Data.Either_ ()
import Smten.Data.Maybe_ ()
import Smten.Data.Tuple_ ()
import Smten.GHC.Num ()
import Smten.GHC.Show ()

-- TODO: Switch to GHC Prelude for these things instead of redefining them.
import Smten.Control.Monad
import Smten.Data.Functor
import Smten.GHC.Enum
import Smten.Data.EnumInteger ()
import Smten.Data.EnumInt ()
import Smten.Data.List
import Smten.Data.Integral
import Smten.Data.Read
import Smten.Smten.Base
import Smten.System.IO

undefined :: a
undefined = error "Prelude.undefined"

{-# NOINLINE primitive #-}
primitive :: String -> a
primitive nm = error ("Smten.primitive: " ++ nm)

