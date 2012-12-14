
module Seri.Prim.Bit (
    bitPs,
    eq_BitP, show_BitP,
    lt_BitP, gt_BitP, leq_BitP, geq_BitP,
    add_BitP, sub_BitP, mul_BitP,
    and_BitP, or_BitP, not_BitP,
    shl_BitP, lshr_BitP,
    fromInteger_BitP, zeroExtend_BitP,
    truncate_BitP, extract_BitP, concat_BitP,
    ) where

import Data.Bits

import Seri.Prim.Prim
import Seri.Type
import Seri.Bit

bitPs :: [Prim]
bitPs = [
    eq_BitP, show_BitP,
    lt_BitP, gt_BitP, leq_BitP, geq_BitP,
    add_BitP, sub_BitP, mul_BitP,
    and_BitP, or_BitP, not_BitP,
    shl_BitP, lshr_BitP,
    fromInteger_BitP, zeroExtend_BitP,
    truncate_BitP, extract_BitP, concat_BitP
    ]

eq_BitP :: Prim
eq_BitP = binaryP "Seri.Bit.__prim_eq_Bit" ((==) :: Bit -> Bit -> Bool)

lt_BitP :: Prim
lt_BitP = binaryP "Seri.Bit.__prim_lt_Bit" ((<) :: Bit -> Bit -> Bool)

gt_BitP :: Prim
gt_BitP = binaryP "Seri.Bit.__prim_gt_Bit" ((>) :: Bit -> Bit -> Bool)

leq_BitP :: Prim
leq_BitP = binaryP "Seri.Bit.__prim_leq_Bit" ((<=) :: Bit -> Bit -> Bool)

geq_BitP :: Prim
geq_BitP = binaryP "Seri.Bit.__prim_geq_Bit" ((>=) :: Bit -> Bit -> Bool)

show_BitP :: Prim
show_BitP = unaryP "Seri.Bit.__prim_show_Bit" (show :: Bit -> String)

fromInteger_BitP :: Prim
fromInteger_BitP =
 let f :: Type -> Integer -> Bit
     f t v =
        let Just w = de_bitT t
        in bv_make w v
 in unaryTP "Seri.Bit.__prim_fromInteger_Bit" f

zeroExtend_BitP :: Prim
zeroExtend_BitP =
 let f :: Type -> Bit -> Bit
     f t v =
        let AppT _ (NumT wt) = t
        in bv_zero_extend (nteval wt - bv_width v) v
 in unaryTP "Seri.Bit.__prim_zeroExtend_Bit" f

add_BitP :: Prim
add_BitP = binaryP "Seri.Bit.__prim_add_Bit" ((+) :: Bit -> Bit -> Bit)

sub_BitP :: Prim
sub_BitP = binaryP "Seri.Bit.__prim_sub_Bit" ((-) :: Bit -> Bit -> Bit)

mul_BitP :: Prim
mul_BitP = binaryP "Seri.Bit.__prim_mul_Bit" ((*) :: Bit -> Bit -> Bit)

and_BitP :: Prim
and_BitP = binaryP "Seri.Bit.__prim_and_Bit" ((.&.) :: Bit -> Bit -> Bit)

or_BitP :: Prim
or_BitP = binaryP "Seri.Bit.__prim_or_Bit" ((.|.) :: Bit -> Bit -> Bit)

shl_BitP :: Prim
shl_BitP = binaryP "Seri.Bit.__prim_shl_Bit" bv_shl

lshr_BitP :: Prim
lshr_BitP = binaryP "Seri.Bit.__prim_lshr_Bit" bv_lshr

not_BitP :: Prim
not_BitP = unaryP "Seri.Bit.__prim_not_Bit" (complement :: Bit -> Bit)

truncate_BitP :: Prim
truncate_BitP =
  let f :: Type -> Bit -> Bit
      f t v =
        let AppT _ (NumT wt) = t
        in bv_truncate (nteval wt) v
  in unaryTP "Seri.Bit.__prim_truncate_Bit" f

extract_BitP :: Prim
extract_BitP =
  let f :: Type -> Bit -> Integer -> Bit
      f t av jv =
        let AppT _ (NumT wt) = t
            i = jv + (nteval wt) - 1
        in bv_extract i jv av
  in binaryTP "Seri.Bit.__prim_extract_Bit" f

concat_BitP :: Prim
concat_BitP = binaryP "Seri.Bit.__prim_concat_Bit" bv_concat
        
