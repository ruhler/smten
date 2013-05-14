
module Smten.HaskellF.Lib.Bit (
    Bit,
    __prim_eq_Bit, __prim_show_Bit,
    __prim_lt_Bit, __prim_gt_Bit, __prim_leq_Bit, __prim_geq_Bit,
    __prim_add_Bit, __prim_sub_Bit, __prim_mul_Bit,
    __prim_fromInteger_Bit, __prim_toInteger_Bit,
    __prim_shl_Bit, __prim_lshr_Bit,
    __prim_or_Bit, __prim_and_Bit, __prim_not_Bit,
    __prim_zeroExtend_Bit, __prim_signExtend_Bit,
    __prim_concat_Bit, __prim_extract_Bit,
    ) where

import Prelude hiding (Bool(..), Integer, String)

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Prim
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude

newtype Bit n = Bit ExpH

instance SmtenT1 Bit where
    smtenT1 _ = ConT bitN (ArrowK NumK StarK)

instance HaskellF1 Bit where
    box1 = Bit
    unbox1 (Bit x) = x


__prim_eq_Bit :: (HaskellF n) => Bit n -> Bit n -> Bool
__prim_eq_Bit = primHF eq_BitP

__prim_show_Bit :: (HaskellF n) => Bit n -> String
__prim_show_Bit = primHF show_BitP

__prim_add_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_add_Bit = primHF add_BitP

__prim_sub_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_sub_Bit = primHF sub_BitP

__prim_mul_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_mul_Bit = primHF mul_BitP

__prim_fromInteger_Bit :: (HaskellF n) => Integer -> Bit n
__prim_fromInteger_Bit = primHF fromInteger_BitP

__prim_toInteger_Bit :: (HaskellF n) => Bit n -> Integer
__prim_toInteger_Bit = primHF toInteger_BitP

__prim_shl_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_shl_Bit = primHF shl_BitP

__prim_lshr_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_lshr_Bit = primHF lshr_BitP

__prim_or_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_or_Bit = primHF or_BitP

__prim_and_Bit :: (HaskellF n) => Bit n -> Bit n -> Bit n
__prim_and_Bit = primHF and_BitP

__prim_not_Bit :: (HaskellF n) => Bit n -> Bit n
__prim_not_Bit = primHF not_BitP

__prim_zeroExtend_Bit :: (HaskellF n, HaskellF m) => Bit n -> Bit m
__prim_zeroExtend_Bit = primHF zeroExtend_BitP

__prim_signExtend_Bit :: (HaskellF n, HaskellF m) => Bit n -> Bit m
__prim_signExtend_Bit = primHF signExtend_BitP

__prim_concat_Bit :: (HaskellF n, HaskellF m, HaskellF n_plus_m)
                     => Bit n -> Bit m -> Bit n_plus_m
__prim_concat_Bit = primHF concat_BitP

__prim_extract_Bit :: (HaskellF n, HaskellF m) => Bit n -> Integer -> Bit m
__prim_extract_Bit = primHF extract_BitP

__prim_lt_Bit :: (HaskellF n) => Bit n -> Bit n -> Bool
__prim_lt_Bit = primHF lt_BitP

__prim_leq_Bit :: (HaskellF n) => Bit n -> Bit n -> Bool
__prim_leq_Bit = primHF leq_BitP

__prim_gt_Bit :: (HaskellF n) => Bit n -> Bit n -> Bool
__prim_gt_Bit = primHF gt_BitP

__prim_geq_Bit :: (HaskellF n) => Bit n -> Bit n -> Bool
__prim_geq_Bit = primHF geq_BitP

