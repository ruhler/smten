
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Bit (smttests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Symbolic
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
--    symtesteq "SMT.Bit.Cmp" (Just 7) [Yices1, Yices2, STP] $ do
--        a <- free
--        assert (a < 8)
--        assert (a > 6)
--        return (a :: Bit #8)
--
--    -- TODO: STP fails this test!
--    symtesteq "SMT.Bit.SignExt1" (Just 3) [Yices1, Yices2] $ do
--        a <- free
--        assert (bv_sign_extend (a :: Bit #3) == (0x3 :: Bit #5))
--        return a
--
--    -- TODO: STP fails this test!
--    symtesteq "SMT.Bit.SignExt2" (Just 4) [Yices1, Yices2] $ do
--        a <- free
--        assert (bv_sign_extend (a :: Bit #3) == (0x1c :: Bit #5))
--        return a

    symtesteq "SMT.Bit.Simple" (Just 0) $ do
        a <- free_Bit
        assert (a == 0)
        return (a :: Bit 5)

    symtesteq "SMT.Bit.Eq" (Just (5, 5)) $ do
        b <- free_Bit
        c <- free_Bit
        assert (b == c)
        assert (c == 5)
        return (b :: Bit 5, c :: Bit 5)

    symtesteq "SMT.Bit.Add" (Just 6) $ do
        d <- free_Bit
        assert ((d + 3) == 1)
        return (d :: Bit 3)

    symtesteq "SMT.Bit.Sub" (Just 1) $ do
        e <- free_Bit
        assert ((e - 3) == 6)
        return (e :: Bit 3)

--    symtesteq "SMT.Bit.Not" (Just 0x0A) [Yices1, Yices2, STP] $ do
--        f <- free
--        assert (bv_not f == 0x15)
--        return (f :: Bit #5)
--
--    -- TODO: this fails for STP
--    symtesteq "SMT.Bit.And" (Just 0x5) [Yices1, Yices2] $ do
--        g <- free
--        assert (bv_and g 0xA == 0x0)
--        assert (bv_or g 0xA == 0xF)
--        return (g :: Bit #4)
--
--    symtesteq "SMT.Bit.Concat" (Just 3) [Yices1, Yices2, STP] $ do
--        h <- free
--        assert (bv_concat (0x5 :: Bit #3) h == 0x17)
--        return (h :: Bit #2)
--
--    -- TODO: this fails for Yices1!
--    symtesteq "SMT.Bit.Lsh" (Just 3) [Yices2, STP] $ do
--        i <- free
--        assert (bv_shl (0x15 :: Bit #8) i == 0xA8)
--        return (i :: Bit #8)
--
--    symtesteq "SMT.Bit.Extract" (Just 0xAB) [Yices1, Yices2, STP] $ do
--        i <- free
--        assert (bv_extract i 0 == (0xB :: Bit #4))
--        assert (bv_extract i 4 == (0xA :: Bit #4))
--        return (i :: Bit #8)
--
--    symtesteq "SMT.Bit.Truncate" (Just 0xAB) [Yices1, Yices2, STP] $ do
--        i <- free
--        assert (bv_truncate i == (0xB :: Bit #4))
--        assert (bv_extract i 4 == (0xA :: Bit #4))
--        return (i :: Bit #8)
--
--    symtesteq "SMT.Bit.Cases" (Just True) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        assert (toInteger (if p then (3 :: Bit #8) else 4) == 3)
--        return p
--
--    symtesteq "SMT.Bit.Cases_Error" (Just (3 :: Bit #8)) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        x <- free_Bit
--        y <- free_Bit
--        let m = if p then Just x else Just y
--        assert (y /= 3)
--        assert (3 == (case m of
--                        Just v -> v
--                        _ -> error "SMT.Bit.Cases_Error"))
--        return x
--
--    symtesteq "SMT.Bit.Cases_Error2" (Just True) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        assert (if p then True
--                     else (3 :: Bit #8) == error "SMT.Bit.Cases_Error2")
--        assert p
--        return p
--
--    symtesteq "SMT.Bit.Cases_Var" (Just (3 :: Bit #8)) [Yices1, Yices2, STP] $ do
--        p <- free_Bool
--        x <- free_Bit
--        y <- free_Bit
--        let m = if p then Just x else Just y
--        assert (y /= 3)
--        assert (3 == (case m of
--                        Just v -> v
--                        _ -> x))
--        return x

