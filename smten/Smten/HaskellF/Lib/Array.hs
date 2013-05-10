
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

primArray :: (HaskellF a) => List__ a -> PrimArray a
primArray = {-# SCC "primArray" #-} primArray'

primArray' :: (HaskellF a) => List__ a -> PrimArray a
primArray' l
  | P.Just vs <- de_listHF l = PrimArray $ listArray (0, genericLength vs -1) vs
  | P.otherwise = P.error "TODO: primArray with symbolic argument"

primSelect :: (HaskellF a) => PrimArray a -> Integer -> a
primSelect = {-# SCC "primSelect" #-} primSelect'

primSelect' :: (HaskellF a) => PrimArray a -> Integer -> a
primSelect' (PrimArray arr) idx
  | P.Just v <- de_smtenHF idx = arr ! v
  | P.otherwise = P.error "TODO: primSelect with symbolic index"
primSelect' _ _ = P.error "TODO: primSelect with symbolic array"

