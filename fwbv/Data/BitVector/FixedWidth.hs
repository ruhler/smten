
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.BitVector.FixedWidth (
    Bit(), 
    width, value, signed_value,
    bvconcat, extract, bvtruncate,
    zero_extend, sign_extend,
    sge, sgt, sle, slt,
    shl, lshr, ashr,
     ) where

import Data.Bits
import Data.Function
import Data.NumT

data Bit n = Bit {
    -- | The value is stored as the smallest non-negative integer representing
    -- the bits of the bit vector.
    value :: Integer
}

width :: forall n . (Numeric n) => Bit n -> Int
width _ = fromInteger $ valueof (numeric :: n)

instance (Numeric n) => Show (Bit n) where
    show b = show (width b) ++ "'d" ++ show (value b)

-- | Construct a bit vector of the given width and value.
-- Truncates the value as appropriate to fit in the given width.
make :: forall n . (Numeric n) => Integer -> Bit n
make v =
  let mask = (shiftL 1 (width (undefined :: Bit n))) - 1
  in Bit (mask .&. v)

instance Eq (Bit n) where
    (==) = (==) `on` value

instance Ord (Bit n) where
    compare = compare `on` value

instance (Numeric n) => Num (Bit n) where
    (+) (Bit a) (Bit b) = make (a+b)
    (*) (Bit a) (Bit b) = make (a*b)
    (-) (Bit a) (Bit b) = make (a-b)
    fromInteger i = make i
    abs = error "todo: abs Bit"
    signum = error "todo: signum Bit"
    
instance (Numeric n) => Bits (Bit n) where
    (.&.) (Bit a) (Bit b) = make (a .&. b)
    (.|.) (Bit a) (Bit b) = make (a .|. b)
    xor (Bit a) (Bit b) = make (a `xor` b)
    complement (Bit a) = make (complement a)
    shift (Bit a) i = make (shift a i)
    rotate = error $ "TODO: Bit rotate"
    bit = error $ "TODO: Bit bit"
    testBit = error $ "TODO: Bit testBit"
    popCount = error $ "TODO: Bit popCount"
    bitSize = width
    isSigned _ = False

zero_extend :: (Numeric m) => Bit n -> Bit m
zero_extend b = make (value b)

sign_extend :: (Numeric n, Numeric m) => Bit n -> Bit m
sign_extend b = make (signed_value b)

signed_value :: Numeric n => Bit n -> Integer
signed_value b@(Bit a) =
  if (testBit a (width b - 1))
    then a - (1 `shiftL` width b)
    else a

bvconcat :: (Numeric n, Numeric m) => Bit n -> Bit m -> Bit (n :+: m)
bvconcat a b =
    let a' = zero_extend a
        b' = zero_extend b
    in (a' `shiftL` width b) .|. b'

-- Extract m bits from the given vector, with the least significant bit to be
-- extracted specified in the argument. The most significant bit is inferred.
extract :: (Numeric n, Numeric m) => Bit n -> Int -> Bit m
extract b l = bvtruncate (b `shiftR` l)

-- Truncate a bit vector to the given number of bits.
bvtruncate :: (Numeric n, Numeric m) => Bit n -> Bit m
bvtruncate b = make (value b)

shl :: Numeric n => Bit n -> Bit n -> Bit n
shl (Bit a) (Bit b) = make (a `shiftL` fromInteger b)

lshr :: Numeric n => Bit n -> Bit n -> Bit n
lshr (Bit a) (Bit b) = make (a `shiftR` fromInteger b)

-- Signed comparisons
slt :: (Numeric n) => Bit n -> Bit n -> Bool
slt a b =
  case (positive a, positive b) of
     (True, False) -> False
     (False, True) -> True
     _ -> a < b

sle :: (Numeric n) => Bit n -> Bit n -> Bool
sle a b = slt a b || a == b

sgt :: (Numeric n) => Bit n -> Bit n -> Bool
sgt a b = not $ sle a b

sge :: (Numeric n) => Bit n -> Bit n -> Bool
sge a b = not $ slt a b

-- Test whether a signed bit vector is positive.
positive :: (Numeric n) => Bit n -> Bool
positive a = extract a (width a - 1) == (0 :: Bit (NumT 1))

-- Arithmetic right shift
ashr :: (Numeric n) => Bit n -> Bit n -> Bit n
ashr a b
  = if positive a
       then lshr a b
       else (lshr a b) .|. (complement (lshr (complement 0) b))

