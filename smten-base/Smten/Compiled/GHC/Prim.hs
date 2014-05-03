
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Smten.Compiled.GHC.Prim (
    GHC.Prim.realWorld#, GHC.Prim.State#, GHC.Prim.RealWorld,
    GHC.Prim.Char#, GHC.Prim.Int#, GHC.Prim.Addr#,
    GHC.Prim.Void#, GHC.Prim.void#,
    (GHC.Prim./=#), (GHC.Prim.==#),
    (GHC.Prim.>=#), (GHC.Prim.<=#), (GHC.Prim.>#), (GHC.Prim.<#),
    GHC.Prim.chr#,
    GHC.Prim.negateInt#, (GHC.Prim.-#), (GHC.Prim.*#), (GHC.Prim.+#),
    GHC.Prim.eqChar#, GHC.Prim.neChar#,
    GHC.Prim.gtChar#, GHC.Prim.geChar#, GHC.Prim.ltChar#, GHC.Prim.leChar#,
    GHC.Prim.ord#,
    tagToEnum#,
    ) where

import qualified GHC.Prim
import Smten.Compiled.GHC.Types

-- Note: this is specialized for Bool so we can support the isTrue#
-- pseudo-primitive operation.
-- Use of tagToEnum for any other type is currently unsupported.
tagToEnum# :: GHC.Prim.Int# -> Bool
tagToEnum# x = if (GHC.Prim.tagToEnum# x) then __True else __False

