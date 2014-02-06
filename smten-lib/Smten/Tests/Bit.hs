
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Bit (tests, FunnyKind(..), my_bv_concat) where

import Smten.Prelude
import Smten.Tests.Test
import Smten.Smten.TypeLits
import Smten.Data.Bit

bit3 :: Integer -> Bit 3
bit3 = fromInteger

bit5 :: Integer -> Bit 5
bit5 = fromInteger

tests :: IO ()
tests = do
    test "Bit.simple" (bit3 0 == 7 + 1)
    test "Bit.sub1" ((bit3 6 - bit3 4) == 2)
    test "Bit.sub2" ((bit3 1 - bit3 3) == 6)
    test "Bit.zeroext" (7 == bv_concat (0 :: Bit 2) (bit3 (0-1)))
    test "Bit.or" (bit5 0x1E == bv_or (bit5 0x12) (bit5 0x0C))
    test "Bit.shl" (bit5 10 == bv_shl (bit5 5) (bit5 1))
    test "Bit.extract" (bit3 3 == bv_extract (bit5 7) 1)
    test "Bit.not" (bit5 0x15 == bv_not (0x0A))
    test "Bit.value" (bv_value (bit5 0x15) == 0x15)
    test "Bit.xor" (0x6 == bv_xor 0xa (0xc :: Bit 4))
    test "Bit.sign lt ++" (bv_slt (bit5 4) 13)
    test "Bit.sign lt +-" (not $ bv_slt (bit5 4) 0x1d)
    test "Bit.sign lt -+" (bv_slt (bit5 0x1c) 3)
    test "Bit.sign lt --" (bv_slt (bit5 0x1c) 0x1d)
    test "Bit.sign le" (bv_sle (bit5 4) 13)
    test "Bit.sign ge" (bv_sge (bit5 13) 4)
    test "Bit.sign gt" (bv_sgt (bit5 13) 4)
    test "Bit.ashr +" (bit5 0x2 == bv_ashr 0x0a 2)
    test "Bit.ashr -" (bit5 0x1e == bv_ashr 0x1a 2)
    test "Bit.sign extend +" (bit5 0x03 == bv_sign_extend (bit3 3))
    test "Bit.sign extend -" (bit5 0x1c == bv_sign_extend (bit3 4))
    test "Bit.my_bv_concat" (0x1c == my_bv_concat (3 :: Bit 2) (4 :: Bit 3))
    test "Bit.read" (bit3 5 == read "5")
    test "Bit.enum" ([bit3 1, bit3 2, bit3 3] == enumFromTo 1 3)
    --test "Bit.arith" ([bit3 1, bit3 2, bit3 3] == [1 .. 3])
    putStrLn "Bit PASSED"


-- Verify we can generate code for data types with this funny kind: (Nat -> *)
data FunnyKind a = FunnyKind (Bit a)

-- Verify we can write a non-primitive function with the (+) type.
my_bv_concat :: (SingI a) => Bit a -> Bit b -> Bit (a+b)
my_bv_concat = bv_concat

