
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld, GHC.Prim.Addr#,
    module Smten.Runtime.Char,
    module Smten.Runtime.Int,
    ) where

import qualified GHC.Prim
import Smten.Runtime.Char
import Smten.Runtime.Int

