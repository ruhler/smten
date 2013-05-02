
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Prim.Array (
    PrimArray(..),
    arrayPs,
    primArrayP, 
    primSelectP,
) where

import Data.Array
import Data.List(genericLength)
import Data.Typeable

import Smten.Lit
import Smten.Type
import Smten.ExpH
import Smten.Prim.Prim

newtype PrimArray = PrimArray {
    prim_array :: Array Integer ExpH
} deriving (Typeable)

arrayEH :: PrimArray -> ExpH
arrayEH x = litEH (dynamicL x)

de_arrayEH :: ExpH -> Maybe PrimArray
de_arrayEH x = do
    l <- de_litEH x
    de_dynamicL l

instance SmtenT PrimArray where
    smtenT _ = error "smtenT on PrimArray"

instance SmtenEH PrimArray where
    smtenEH = arrayEH
    de_smtenEH = de_arrayEH

arrayPs :: [Prim]
arrayPs = [p_prim primArrayP, p_prim primSelectP]

primArrayP :: PrimF ([ExpH] -> PrimArray)
primArrayP =
  let f :: [ExpH] -> PrimArray
      f xs = PrimArray $ listArray (0, genericLength xs - 1) xs
  in unaryPF "Smten.PrimArray.primArray" f

primSelectP :: PrimF (PrimArray -> Integer -> ExpH)
primSelectP = binaryPF "Smten.PrimArray.primSelect" ((!) . prim_array )

