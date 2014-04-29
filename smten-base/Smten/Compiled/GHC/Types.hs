
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Smten.Compiled.GHC.Types (
    module Smten.Compiled.Smten.Data.Ordering,
    Bool, __True, __False,
    Char(..), __C#, __applyToChar,
    Int(..), __I#, __applyToInt,
    IO, __deNewTyIO,
 ) where

import GHC.Prim (RealWorld, State#)
import GHC.Types (IO(..))
import Smten.Runtime.Formula
import Smten.Compiled.Smten.Data.Ordering
import Smten.Compiled.Smten.Smten.Base

type Bool = BoolF

__True :: Bool
__True = trueF

__False :: Bool
__False = falseF

__deNewTyIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
__deNewTyIO (IO a) = a

