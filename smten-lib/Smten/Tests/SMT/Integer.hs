
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.SMT.Integer(smttests) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Search
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
    symtesteq "SMT.Integer.Trivial" (Just 0) $ do
        a <- free_Integer
        guard (a == 0)
        return a

    symtesteq "SMT.Integer.Eq" (Just (4, 4)) $ do
        b <- free_Integer
        c <- free_Integer
        guard (b == c)
        guard (b == 4)
        return (b, c)

    symtesteq "SMT.Integer.Add" (Just (6, 8)) $ do
        d <- free_Integer
        e <- free_Integer
        guard ((d + 2) == e)
        guard (e == 8)
        return (d, e)

    symtesteq "SMT.Integer.Sub" (Just (10, 8)) $ do
        f <- free_Integer
        g <- free_Integer
        guard ((f - 2) == g)
        guard (g == 8)
        return (f, g)

    symtesteq "SMT.Integer.Compare" (Just 7) $ do
        h <- free_Integer
        guard (h < 8)
        guard (h > 6)
        return h

    symtesteq "SMT.Integer.Compare2" (Just 7) $ do
        i <- free_Integer
        guard (i >= 7)
        guard (i <= 7)
        return i

    -- Test more complex
    symtesteq "SMT.Integer.Complex" (Just (5, 3)) $ do
        x <- free_Integer
        y <- free_Integer
        guard ((x + y) == 8)
        guard ((x - y) == 2)
        return (x, y)

    -- Test realization of an Int from an integer
    -- This should work fine, without any performance issues, because we
    -- already know what the integer is
    symtesteq "SMT.integer2int" (Just (5 :: Int)) $ do
       x <- free_Integer
       guard (x == 5)
       return (fromInteger x)

    symtesteq "SMT.integer2int.zero" (Just (0 :: Int)) $ do
       x <- free_Integer
       guard (x == 0)
       return (fromInteger x)

    symtesteq "SMT.integer2int.negative" (Just (negate 5 :: Int)) $ do
       x <- free_Integer
       guard (x == negate 5)
       return (fromInteger x)

    -- At one point this caused a stack overflow.
    symtesteq "SMT.Integer.SMul" Nothing $ do
       x <- mplus (return 0) (return 1)
       guard (((x+1) * 2) /= ((x+1) * 2))
       return x
    

