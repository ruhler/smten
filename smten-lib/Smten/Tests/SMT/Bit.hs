
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.SMT.Bit (smttests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Search
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
    symtesteq "SMT.Bit.Simple" (Just 0) $ do
        a <- free_Bit
        guard (a == 0)
        return (a :: Bit 5)

    symtesteq "SMT.Bit.Eq" (Just (5, 5)) $ do
        b <- free_Bit
        c <- free_Bit
        guard (b == c)
        guard (c == 5)
        return (b :: Bit 5, c :: Bit 5)

    symtesteq "SMT.Bit.Add" (Just 6) $ do
        d <- free_Bit
        guard ((d + 3) == 1)
        return (d :: Bit 3)

    symtesteq "SMT.Bit.Sub" (Just 1) $ do
        e <- free_Bit
        guard ((e - 3) == 6)
        return (e :: Bit 3)

    symtesteq "SMT.Bit.Cmp" (Just 7) $ do
        a <- free_Bit
        guard (a < 8)
        guard (a > 6)
        return (a :: Bit 8)

    symtesteq "SMT.Bit.SignExt1" (Just 3) $ do
        a <- free_Bit
        guard (bv_sign_extend (a :: Bit 3) == (0x3 :: Bit 5))
        return a

    symtesteq "SMT.Bit.SignExt2" (Just 4) $ do
        a <- free_Bit
        guard (bv_sign_extend (a :: Bit 3) == (0x1c :: Bit 5))
        return a

    symtesteq "SMT.Bit.Not" (Just 0x0A) $ do
        f <- free_Bit
        guard (bv_not f == 0x15)
        return (f :: Bit 5)

    symtesteq "SMT.Bit.And" (Just 0x5) $ do
        g <- free_Bit
        guard (bv_and g 0xA == 0x0)
        guard (bv_or g 0xA == 0xF)
        return (g :: Bit 4)

    symtesteq "SMT.Bit.Concat" (Just 3) $ do
        h <- free_Bit
        guard (bv_concat (0x5 :: Bit 3) h == (0x17 :: Bit 5))
        return (h :: Bit 2)

    symtesteq "SMT.Bit.Lsh" (Just 3) $ do
        i <- free_Bit
        guard (bv_shl (0x15 :: Bit 8) i == 0xA8)
        return (i :: Bit 8)

    symtesteq "SMT.Bit.Lshr" (Just 3) $ do
        i <- free_Bit
        guard (bv_lshr (0xA8 :: Bit 8) i == 0x15)
        return (i :: Bit 8)

    symtesteq "SMT.Bit.Extract" (Just 0xAB) $ do
        i <- free_Bit
        guard (bv_extract i 0 == (0xB :: Bit 4))
        guard (bv_extract i 4 == (0xA :: Bit 4))
        return (i :: Bit 8)

    -- this was a bug in the Bits wrapper
    symtesteq "SMT.Bit.Extract2" (Just 0xAB) $ do
        i <- free_Bit
        guard (bv_extract i 0 == (0xB :: Bit 4))
        guard (bv_extract i 4 == (0xA :: Bit 4))
        guard (bv_extract i 4 <= (0xF :: Bit 4))
        return (i :: Bit 8)

    symtesteq "SMT.Bit.Truncate" (Just 0xAB) $ do
        i <- free_Bit
        guard (bv_truncate i == (0xB :: Bit 4))
        guard (bv_extract i 4 == (0xA :: Bit 4))
        return (i :: Bit 8)

    symtesteq "SMT.Bit.Large" (Just 0x123456789ABCDEFEDCBA9876543210123) $ do
        i <- free_Bit
        guard (i == (0x123456789ABCDEFEDCBA9876543210123 :: Bit 132))
        return i

    -- This test case failed for the Bits wrapper
    symtesteq "SMT.Bit.BitsBug" Nothing $ do
        x <- free_Bit
        guard (x < (4 :: Bit 12))
        guard (all (/= x) [0..3])
        return x

    symtesteq "SMT.Bit.udiv/urem" (Just (9 :: Bit 8)) $ do
        x <- free_Bit
        guard ((x `bv_udiv` 2) == 4)
        guard ((x `bv_urem` 2) == 1)
        return x
        

--    symtesteq "SMT.Bit.Cases" (Just True) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        guard (toInteger (if p then (3 :: Bit #8) else 4) == 3)
--        return p
--
--    symtesteq "SMT.Bit.Cases_Error" (Just (3 :: Bit #8)) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        x <- free_Bit
--        y <- free_Bit
--        let m = if p then Just x else Just y
--        guard (y /= 3)
--        guard (3 == (case m of
--                        Just v -> v
--                        _ -> error "SMT.Bit.Cases_Error"))
--        return x
--
--    symtesteq "SMT.Bit.Cases_Error2" (Just True) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        guard (if p then True
--                     else (3 :: Bit #8) == error "SMT.Bit.Cases_Error2")
--        guard p
--        return p
--
--    symtesteq "SMT.Bit.Cases_Var" (Just (3 :: Bit #8)) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        x <- free_Bit
--        y <- free_Bit
--        let m = if p then Just x else Just y
--        guard (y /= 3)
--        guard (3 == (case m of
--                        Just v -> v
--                        _ -> x))
--        return x

