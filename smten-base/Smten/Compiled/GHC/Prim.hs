
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld, GHC.Prim.Addr#,
    module Smten.Compiled.Smten.Smten.PrimChar,
    module Smten.Compiled.Smten.Smten.PrimInt,
    ord#, chr#,
    ) where

import qualified GHC.Prim
import Smten.Compiled.Smten.Smten.PrimChar
import Smten.Compiled.Smten.Smten.PrimInt

ord# :: Char# -> Int#
ord# c = primCharApp (\v -> int# (GHC.Prim.ord# v)) c

chr# :: Int# -> Char#
chr# i = primIntApp (\v -> char# (GHC.Prim.chr# v)) i

