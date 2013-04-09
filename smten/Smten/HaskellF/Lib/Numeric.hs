
module Smten.HaskellF.Lib.Numeric (
    N__0, N__2p1, N__2p0, N__PLUS, N__MINUS, N__TIMES,
    ) where

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.HaskellF.HaskellF

newtype N__0 = N__0 ExpH

instance SmtenT N__0 where
    smtenT _ = NumT 0

instance HaskellF N__0 where
    box = N__0
    unbox (N__0 x) = x

newtype N__1 = N__1 ExpH

instance SmtenT N__1 where
    smtenT _ = NumT 1

instance HaskellF N__1 where
    box = N__1
    unbox (N__1 x) = x

newtype N__2 = N__2 ExpH

instance SmtenT N__2 where
    smtenT _ = NumT 2

instance HaskellF N__2 where
    box = N__2
    unbox (N__2 x) = x

newtype N__PLUS a b = N__PLUS ExpH

instance SmtenT2 N__PLUS where
    smtenT2 _ = ConT (name "+") (ArrowK (ArrowK NumK NumK) NumK)

instance HaskellF2 N__PLUS where
    box2 = N__PLUS
    unbox2 (N__PLUS x) = x

newtype N__MINUS a b = N__MINUS ExpH

instance SmtenT2 N__MINUS where
    smtenT2 _ = ConT (name "-") (ArrowK (ArrowK NumK NumK) NumK)

instance HaskellF2 N__MINUS where
    box2 = N__MINUS
    unbox2 (N__MINUS x) = x

newtype N__TIMES a b = N__TIMES ExpH

instance SmtenT2 N__TIMES where
    smtenT2 _ = ConT (name "*") (ArrowK (ArrowK NumK NumK) NumK)

instance HaskellF2 N__TIMES where
    box2 = N__TIMES
    unbox2 (N__TIMES x) = x

type N__2p0 a = N__TIMES N__2 a
type N__2p1 a = N__PLUS (N__2p0 a) N__1
    

