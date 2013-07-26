
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.Bit (tests) where

import Smten.Prelude
import Smten.Tests.Test
import Smten.Data.Bit

bit3 :: Integer -> Bit 3
bit3 = fromInteger

bit5 :: Integer -> Bit 5
bit5 = fromInteger

tests :: IO ()
tests = do
    test "Bit.simple" (bit3 0 == 7 + 1)
--    test "Bit.OR" ((3 :: Bit #3) == (bv_or 1 2))
--    test "Bit.zeroext" (bit5 7 == bv_zero_extend (bit3 (0-1)))
--    test "Bit.or" (bit5 0x1E == bv_or (bit5 0x12) (bit5 0x0C))
--    test "Bit.shl" (bit5 10 == bv_shl (bit5 5) (bit5 1))
--    test "Bit.extract" (bit3 3 == bv_extract (bit5 7) 1)
--    test "Bit.not" (bit5 0x15 == bv_not (0x0A))
--    test "Bit.toInteger" (toInteger (bit5 0x15) == 0x15)
--    test "Bit.xor" (0x6 == bv_xor 0xa (0xc :: Bit #4))
--    test "Bit.sign lt ++" (bv_slt (bit5 4) 13)
--    test "Bit.sign lt +-" (not $ bv_slt (bit5 4) 0x1d)
--    test "Bit.sign lt -+" (bv_slt (bit5 0x1c) 3)
--    test "Bit.sign lt --" (bv_slt (bit5 0x1c) 0x1d)
--    test "Bit.sign le" (bv_sle (bit5 4) 13)
--    test "Bit.sign ge" (bv_sge (bit5 13) 4)
--    test "Bit.sign gt" (bv_sgt (bit5 13) 4)
--    test "Bit.ashr +" (bit5 0x2 == bv_ashr 0x0a 2)
--    test "Bit.ashr -" (bit5 0x1e == bv_ashr 0x1a 2)
--    test "Bit.sign extend +" (bit5 0x03 == bv_sign_extend (bit3 3))
--    test "Bit.sign extend -" (bit5 0x1c == bv_sign_extend (bit3 4))
--    test "Bit.pattern" (
--        case bit3 3 of
--            3 -> True
--            _ -> False
--      )
    putStrLn "Bit PASSED"


