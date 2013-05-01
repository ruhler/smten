
module Smten.HaskellF.Lib.PrimArray (
    PrimArray, primArray, primSelect,
    ) where

import Prelude hiding (Integer)

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Prim
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude

newtype PrimArray a = PrimArray ExpH

instance SmtenT1 PrimArray where
    smtenT1 _ = ConT primArrayN (ArrowK NumK StarK)

instance HaskellF1 PrimArray where
    box1 = PrimArray
    unbox1 (PrimArray x) = x

primArray :: (HaskellF a) => List__ a -> PrimArray a
primArray = primHF (p_prim primArrayP)

primSelect :: (HaskellF a) => PrimArray a -> Integer -> a
primSelect = primHF (p_prim primSelectP)

