
module Seri.Prim.Bit (
    bitPs,
    eq_BitP, show_BitP,
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
    add_BitP, sub_BitP, mul_BitP,
    and_BitP, or_BitP, not_BitP,
    shl_BitP, lshr_BitP,
    fromInteger_BitP, zeroExtend_BitP,
    truncate_BitP, extract_BitP, concat_BitP
    ]

eq_BitP :: Prim
eq_BitP = binaryP "Seri.Bit.__prim_eq_Bit" ((==) :: Bit -> Bit -> Bool)

show_BitP :: Prim
show_BitP = unaryP "Seri.Bit.__prim_show_Bit" (show :: Bit -> String)

fromInteger_BitP :: Prim
fromInteger_BitP =
 let f :: Type -> Integer -> Bit
     f t v =
        let Just (_, bt) = de_arrowT t
            Just w = de_bitT bt
        in bv_make w v
 in unaryTP "Seri.Bit.__prim_fromInteger_Bit" f

zeroExtend_BitP :: Prim
zeroExtend_BitP =
 let f :: Type -> Bit -> Bit
     f t v =
        let [ta, AppT _ (NumT wt)] = de_arrowsT t
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
        let [ta, AppT _ (NumT wt)] = de_arrowsT t
        in bv_truncate (nteval wt) v
  in unaryTP "Seri.Bit.__prim_truncate_Bit" f

extract_BitP :: Prim
extract_BitP =
  let f :: Type -> Bit -> Integer -> Bit
      f t av jv =
        let AppT _ (NumT wt) = last $ de_arrowsT t
            i = jv + (nteval wt) - 1
        in bv_extract i jv av
  in binaryTP "Seri.Bit.__prim_extract_Bit" f

concat_BitP :: Prim
concat_BitP = binaryP "Seri.Bit.__prim_concat_Bit" bv_concat
        
