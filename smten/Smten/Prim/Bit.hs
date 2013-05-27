
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Prim.Bit where

import Data.Bits

import Smten.Prim.Prim
import Smten.Type
import Smten.Bit

__prim_eq_BitP :: Prim
__prim_eq_BitP = binaryP "Smten.Bit.__prim_eq_Bit" ((==) :: Bit -> Bit -> Bool)

__prim_lt_BitP :: Prim
__prim_lt_BitP = binaryP "Smten.Bit.__prim_lt_Bit" ((<) :: Bit -> Bit -> Bool)

__prim_gt_BitP :: Prim
__prim_gt_BitP = binaryP "Smten.Bit.__prim_gt_Bit" ((>) :: Bit -> Bit -> Bool)

__prim_leq_BitP :: Prim
__prim_leq_BitP = binaryP "Smten.Bit.__prim_leq_Bit" ((<=) :: Bit -> Bit -> Bool)

__prim_geq_BitP :: Prim
__prim_geq_BitP = binaryP "Smten.Bit.__prim_geq_Bit" ((>=) :: Bit -> Bit -> Bool)

__prim_show_BitP :: Prim
__prim_show_BitP = unaryP "Smten.Bit.__prim_show_Bit" (show :: Bit -> String)

__prim_fromInteger_BitP :: Prim
__prim_fromInteger_BitP =
 let f :: Type -> Integer -> Bit
     f t v =
        let Just w = de_bitT t
        in bv_make w v
 in unaryTP "Smten.Bit.__prim_fromInteger_Bit" f

__prim_toInteger_BitP :: Prim
__prim_toInteger_BitP = unaryP "Smten.Bit.__prim_toInteger_Bit" bv_value

__prim_zeroExtend_BitP :: Prim
__prim_zeroExtend_BitP =
 let f :: Type -> Bit -> Bit
     f t v =
        let AppT _ wt = t
        in bv_zero_extend (nteval wt - bv_width v) v
 in unaryTP "Smten.Bit.__prim_zeroExtend_Bit" f

__prim_signExtend_BitP :: Prim
__prim_signExtend_BitP =
 let f :: Type -> Bit -> Bit
     f t v =
        let AppT _ wt = t
        in bv_sign_extend (nteval wt - bv_width v) v
 in unaryTP "Smten.Bit.__prim_signExtend_Bit" f

__prim_add_BitP :: Prim
__prim_add_BitP = binaryP "Smten.Bit.__prim_add_Bit" ((+) :: Bit -> Bit -> Bit)

__prim_sub_BitP :: Prim
__prim_sub_BitP = binaryP "Smten.Bit.__prim_sub_Bit" ((-) :: Bit -> Bit -> Bit)

__prim_mul_BitP :: Prim
__prim_mul_BitP = binaryP "Smten.Bit.__prim_mul_Bit" ((*) :: Bit -> Bit -> Bit)

__prim_and_BitP :: Prim
__prim_and_BitP = binaryP "Smten.Bit.__prim_and_Bit" ((.&.) :: Bit -> Bit -> Bit)

__prim_or_BitP :: Prim
__prim_or_BitP = binaryP "Smten.Bit.__prim_or_Bit" ((.|.) :: Bit -> Bit -> Bit)

__prim_shl_BitP :: Prim
__prim_shl_BitP = binaryP "Smten.Bit.__prim_shl_Bit" bv_shl

__prim_lshr_BitP :: Prim
__prim_lshr_BitP = binaryP "Smten.Bit.__prim_lshr_Bit" bv_lshr

__prim_not_BitP :: Prim
__prim_not_BitP = unaryP "Smten.Bit.__prim_not_Bit" (complement :: Bit -> Bit)

__prim_extract_BitP :: Prim
__prim_extract_BitP =
  let f :: Type -> Bit -> Integer -> Bit
      f t av jv =
        let AppT _ wt = t
            i = jv + (nteval wt) - 1
        in bv_extract i jv av
  in binaryTP "Smten.Bit.__prim_extract_Bit" f

__prim_concat_BitP :: Prim
__prim_concat_BitP = binaryP "Smten.Bit.__prim_concat_Bit" bv_concat
        
