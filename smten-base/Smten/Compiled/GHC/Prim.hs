
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld,
    GHC.Prim.Char#, GHC.Prim.Int#, GHC.Prim.Addr#,
    (/=#), (==#), (>=#), (<=#), (>#), (<#),
    eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
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

eqChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
eqChar# a b = if (GHC.Prim.eqChar# a b) then __True else __False

neChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
neChar# a b = if (GHC.Prim.neChar# a b) then __True else __False

gtChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
gtChar# a b = if (GHC.Prim.gtChar# a b) then __True else __False

geChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
geChar# a b = if (GHC.Prim.geChar# a b) then __True else __False

ltChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
ltChar# a b = if (GHC.Prim.ltChar# a b) then __True else __False

leChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
leChar# a b = if (GHC.Prim.leChar# a b) then __True else __False

