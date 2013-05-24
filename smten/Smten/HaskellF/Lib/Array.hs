
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Lib.Array (
    PrimArray, primArray, primSelect,
    ) where

import Prelude hiding (Integer)
import qualified Prelude as P
import Data.Array
import Data.List(genericLength)

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Prim.Prim
import qualified Smten.Prim.Array as PA
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude

data PrimArray a = PrimArray__s ExpH

instance SmtenT1 PrimArray where
    smtenT1 _ = ConT primArrayN (ArrowK NumK StarK)

instance HaskellF1 PrimArray where
    box1 = PrimArray__s
    unbox1 (PrimArray__s x) = x

primArray :: (HaskellF a) => Function (List__ a) (PrimArray a)
primArray = primHF (p_prim PA.primArrayP)
    
primSelect :: (HaskellF a) => Function (PrimArray a) (Function Integer a)
primSelect = primHF (p_prim PA.primSelectP)

