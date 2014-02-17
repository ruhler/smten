
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld,
    GHC.Prim.Char#, GHC.Prim.Int#, GHC.Prim.Addr#,
    (/=#), (==#), (>=#), (<=#), (>#), (<#),
    GHC.Prim.negateInt#, (GHC.Prim.-#), (GHC.Prim.*#), (GHC.Prim.+#),
    eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
    ) where

import qualified GHC.Prim
import Smten.Compiled.GHC.Types

(/=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(/=#) a b = {-# SCC "PRIM_INT_NE" #-} if (a GHC.Prim./=# b) then __True else __False

(==#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(==#) a b = {-# SCC "PRIM_INT_EQ" #-} if (a GHC.Prim.==# b) then __True else __False

(>=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(>=#) a b = {-# SCC "PRIM_INT_GE" #-} if (a GHC.Prim.>=# b) then __True else __False

(<=#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(<=#) a b = {-# SCC "PRIM_INT_LE" #-} if (a GHC.Prim.<=# b) then __True else __False

(>#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(>#) a b = {-# SCC "PRIM_INT_GT" #-} if (a GHC.Prim.># b) then __True else __False

(<#) :: GHC.Prim.Int# -> GHC.Prim.Int# -> Bool
(<#) a b = {-# SCC "PRIM_INT_LT" #-} if (a GHC.Prim.<# b) then __True else __False

eqChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
eqChar# a b = {-# SCC "PRIM_CHAR_EQ" #-} if (GHC.Prim.eqChar# a b) then __True else __False

neChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
neChar# a b = {-# SCC "PRIM_CHAR_NE" #-} if (GHC.Prim.neChar# a b) then __True else __False

gtChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
gtChar# a b = {-# SCC "PRIM_CHAR_GT" #-} if (GHC.Prim.gtChar# a b) then __True else __False

geChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
geChar# a b = {-# SCC "PRIM_CHAR_GE" #-} if (GHC.Prim.geChar# a b) then __True else __False

ltChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
ltChar# a b = {-# SCC "PRIM_CHAR_LT" #-} if (GHC.Prim.ltChar# a b) then __True else __False

leChar# :: GHC.Prim.Char# -> GHC.Prim.Char# -> Bool
leChar# a b = {-# SCC "PRIM_CHAR_LE" #-} if (GHC.Prim.leChar# a b) then __True else __False

