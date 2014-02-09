
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld,
    GHC.Prim.Char#, GHC.Prim.Int#, GHC.Prim.Addr#,
    (/=#), (==#), (>=#), (<=#), (>#), (<#),
    ) where

import qualified GHC.Prim
import Smten.Compiled.Smten.Data.Bool0

(/=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(/=#) a b = if (a GHC.Prim./=# b) then __True else __False

(==#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(==#) a b = if (a GHC.Prim.==# b) then __True else __False

(>=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(>=#) a b = if (a GHC.Prim.>=# b) then __True else __False

(<=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(<=#) a b = if (a GHC.Prim.<=# b) then __True else __False

(>#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(>#) a b = if (a GHC.Prim.># b) then __True else __False

(<#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(<#) a b = if (a GHC.Prim.<# b) then __True else __False

