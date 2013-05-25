
{-# LANGUAGE EmptyDataDecls #-}

module Smten.HaskellF.Numeric (
    N__0, N__2p1, N__2p0, N__PLUS, N__MINUS, N__TIMES,
    ) where

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.HaskellF.HaskellF

data N__0
data N__1
data N__2
data N__PLUS a b
data N__MINUS a b
data N__TIMES a b
type N__2p0 a = N__TIMES N__2 a
type N__2p1 a = N__PLUS (N__2p0 a) N__1

instance SmtenT N__0 where
    smtenT _ = NumT 0

instance SmtenT N__1 where
    smtenT _ = NumT 1

instance SmtenT N__2 where
    smtenT _ = NumT 2

instance SmtenT2 N__PLUS where
    smtenT2 _ = ConT (name "+") (ArrowK (ArrowK NumK NumK) NumK)

instance SmtenT2 N__MINUS where
    smtenT2 _ = ConT (name "-") (ArrowK (ArrowK NumK NumK) NumK)

instance SmtenT2 N__TIMES where
    smtenT2 _ = ConT (name "*") (ArrowK (ArrowK NumK NumK) NumK)

