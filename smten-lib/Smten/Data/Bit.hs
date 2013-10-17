
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Data.Bit (
    Bit,
    bv_and, bv_or, bv_xor, bv_shl, bv_lshr, bv_not, bv_concat,
    bv_extract, bv_truncate, bv_sign_extend,
    bv_sle, bv_slt, bv_sge, bv_sgt, bv_ashr, bv_width, bv_value,
    ) where

import Smten.Prelude
import Smten.Smten.TypeLits
import Smten.Data.Bit0
import Smten.Data.Ix

instance (SingI n) => Eq (Bit n) where
    (==) = bv_eq

instance (SingI n) => Ord (Bit n) where
    (<=) = bv_leq

instance (SingI n) => Show (Bit n) where
    show = bv_show

instance (SingI n) => Read (Bit n) where
    readsPrec p x = [(fromInteger n, s) | (n, s) <- readsPrec p x]

instance (SingI n) => Num (Bit n) where
    (+) = bv_add
    (-) = bv_sub
    (*) = bv_mul
    abs = error "TODO: Bit.abs"
    signum = error "TODO: Bit.signum"
    fromInteger = bv_fromInteger

bv_xor :: Bit n -> Bit n -> Bit n
bv_xor a b = bv_and (bv_or a b) (bv_not (bv_and a b))

--  There are numeric type issues with this function.
--  Use instead (bv_concat 0)
--bv_zero_extend :: forall n m . (SingI m) => Bit n -> Bit (m+n)
--bv_zero_extend x = bv_concat (0 :: Bit m) x

bv_truncate :: (SingI m, SingI n) => Bit m -> Bit n
bv_truncate x = bv_extract x 0

bv_slt :: (SingI n) => Bit n -> Bit n -> Bool
bv_slt a b =
  case (bv_positive a, bv_positive b) of
     (True, False) -> False
     (False, True) -> True
     _ -> a < b

bv_sle :: (SingI n) => Bit n -> Bit n -> Bool
bv_sle a b = bv_slt a b || a == b

bv_sge :: (SingI n) => Bit n -> Bit n -> Bool
bv_sge a b = not $ bv_slt a b

bv_sgt :: (SingI n) => Bit n -> Bit n -> Bool
bv_sgt a b = not $ bv_sle a b

bv_ashr :: (SingI n) => Bit n -> Bit n -> Bit n
bv_ashr a b
  = if bv_positive a
        then bv_lshr a b
        else bv_or (bv_lshr a b) (bv_not (bv_lshr (bv_not 0) b))

-- Test whether a signed bit vector is positive.
bv_positive :: (SingI n) => Bit n -> Bool
bv_positive a = bv_extract a (bv_width a - 1) == (0 :: Bit 1)

instance (SingI n) => Ix (Bit n) where
    range (l, h) = 
      case (l > h, l == h) of
          (True, _) -> []
          (_, True) -> [h]
          _ -> l : range (l + 1, h)

    index (l, _) x = fromInteger (bv_value (x - l))
