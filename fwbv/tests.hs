
{-# LANGUAGE DataKinds #-}

import Data.Bits
import Data.NumT
import Data.BitVector.FixedWidth

test :: String -> Bool -> IO ()
test _ True = return ()
test msg False = error msg

bit3 :: Integer -> Bit (NumT 3)
bit3 = fromInteger

bit5 :: Integer -> Bit (NumT 5)
bit5 = fromInteger

main :: IO ()
main = do
    test "Bit.Simple" (bit3 0 == 7 + 1)
    test "Bit.OR" (bit3 3 == (1 .|. 2))
    test "Bit.simple" (bit3 0 == ((bit3 7) + 1))
    test "Bit.zeroext" (bit5 7 == zero_extend (bit3 (0-1)))
    test "Bit.or" (bit5 0x1E == (bit5 0x12) .|. (bit5 0x0C))
    test "Bit.shl" (bit5 10 == shl (bit5 5) (bit5 1))
    test "Bit.extract" (bit3 3 == extract (bit5 7) 1)
    test "Bit.not" (bit5 0x15 == complement (0x0A))
    test "Bit.xor" (0x6 == xor 0xa (0xc :: Bit (NumT 4)))
    test "Bit.sign lt ++" (slt (bit5 4) 13)
    test "Bit.sign lt +-" (not $ slt (bit5 4) 0x1d)
    test "Bit.sign lt -+" (slt (bit5 0x1c) 3)
    test "Bit.sign lt --" (slt (bit5 0x1c) 0x1d)
    test "Bit.sign le" (sle (bit5 4) 13)
    test "Bit.sign ge" (sge (bit5 13) 4)
    test "Bit.sign gt" (sgt (bit5 13) 4)
    test "Bit.ashr +" (bit5 0x2 == ashr 0x0a 2)
    test "Bit.ashr -" (bit5 0x1e == ashr 0x1a 2)
    test "Bit.sign extend +" (bit5 0x03 == sign_extend (bit3 3))
    test "Bit.sign extend -" (bit5 0x1c == sign_extend (bit3 4))
    putStrLn "Bit PASSED"

