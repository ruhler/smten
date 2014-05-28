
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Smten.Compiled.GHC.Types (
    module Smten.Compiled.Smten.Data.Ordering,
    module Smten.Runtime.Bool,
    module Smten.Compiled.Smten.Smten.Char,
    module Smten.Compiled.Smten.Smten.Int,
    IO, __deNewTyIO,
 ) where

import GHC.Prim (RealWorld, State#)
import GHC.Types (IO(..))
import Smten.Compiled.Smten.Data.Ordering
import Smten.Runtime.Bool
import Smten.Compiled.Smten.Smten.Char
import Smten.Compiled.Smten.Smten.Int

__deNewTyIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
__deNewTyIO (IO a) = a

