
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Integer(smttests) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
    symtesteq "SMT.Integer.Trivial" (Just 0) $ do
        a <- free_Integer
        assert (a == 0)
        return a

    symtesteq "SMT.Integer.Eq" (Just (4, 4)) $ do
        b <- free_Integer
        c <- free_Integer
        assert (b == c)
        assert (b == 4)
        return (b, c)

    symtesteq "SMT.Integer.Add" (Just (6, 8)) $ do
        d <- free_Integer
        e <- free_Integer
        assert ((d + 2) == e)
        assert (e == 8)
        return (d, e)

    symtesteq "SMT.Integer.Sub" (Just (10, 8)) $ do
        f <- free_Integer
        g <- free_Integer
        assert ((f - 2) == g)
        assert (g == 8)
        return (f, g)

    symtesteq "SMT.Integer.Compare" (Just 7) $ do
        h <- free_Integer
        assert (h < 8)
        assert (h > 6)
        return h

    symtesteq "SMT.Integer.Compare2" (Just 7) $ do
        i <- free_Integer
        assert (i >= 7)
        assert (i <= 7)
        return i

    -- Test more complex
    symtesteq "SMT.Integer.Complex" (Just (5, 3)) $ do
        x <- free_Integer
        y <- free_Integer
        assert ((x + y) == 8)
        assert ((x - y) == 2)
        return (x, y)

    -- Test realization of an Int from an integer
    -- This should work fine, without any performance issues, because we
    -- already know what the integer is
    symtesteq "SMT.integer2int" (Just (5 :: Int)) $ do
       x <- free_Integer
       assert (x == 5)
       return (fromInteger x)

    symtesteq "SMT.integer2int.zero" (Just (0 :: Int)) $ do
       x <- free_Integer
       assert (x == 0)
       return (fromInteger x)

    symtesteq "SMT.integer2int.negative" (Just (negate 5 :: Int)) $ do
       x <- free_Integer
       assert (x == negate 5)
       return (fromInteger x)
