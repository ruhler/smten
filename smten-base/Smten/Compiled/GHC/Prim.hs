
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld, GHC.Prim.Addr#,
    module Smten.Runtime.Char,
    module Smten.Runtime.Int,
    ord#, chr#,
    ) where

import qualified GHC.Prim
import Smten.Runtime.Char
import Smten.Runtime.Int

ord# :: Char# -> Int#
ord# c = primCharApp (\v -> int# (GHC.Prim.ord# v)) c

chr# :: Int# -> Char#
chr# i = primIntApp (\v -> char# (GHC.Prim.chr# v)) i

