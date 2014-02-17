
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module Smten.Compiled.GHC.Types (
    module Smten.Compiled.Smten.Data.Ordering,
    Bool, __True, __False,
    Char(..), __C#, __I#, Int(..), IO,
 ) where

import GHC.Types (IO)
import Smten.Runtime.Formula
import Smten.Compiled.Smten.Data.Ordering
import Smten.Compiled.Smten.Smten.Base

__C# = C#
__I# = I#

type Bool = BoolF

__True :: Bool
__True = trueF

__False :: Bool
__False = falseF

