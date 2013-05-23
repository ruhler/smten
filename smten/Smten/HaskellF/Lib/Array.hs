
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
import qualified Smten.Prim.Array as PA
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude

data PrimArray a =
       PrimArray (Array P.Integer a)
     | PrimArray__s ExpH

instance SmtenT1 PrimArray where
    smtenT1 _ = ConT primArrayN (ArrowK NumK StarK)

instance HaskellF1 PrimArray where
    box1 = PrimArray__s
    unbox1 = P.error "TODO: PrimArray unbox1"

primArray :: (HaskellF a) => Function (List__ a) (PrimArray a)
primArray = {-# SCC "primArray" #-} lamHF "l" $ \l ->
    case de_listHF l of
        P.Just vs -> PrimArray $ listArray (0, genericLength vs -1) vs
        _ -> P.error "TODO: primArray with symbolic argument"
    
primSelect :: (HaskellF a) => Function (PrimArray a) (Function Integer a)
primSelect = {-# SCC "primSelect" #-} lamHF "par" $ \par ->
    lamHF "idx" $ \idx ->
        case (par, de_smtenHF idx) of
            (PrimArray arr, P.Just v) -> arr ! v
            (PrimArray arr, _) -> P.error "TODO: primSelect with symbolic index"
            _ -> P.error "TODO: primSelect with symbolic array"

