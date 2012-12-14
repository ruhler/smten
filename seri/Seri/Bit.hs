-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

-- | Bit vector library. Widths are specified dynamically instead of enforced
-- statically to avoid complications with numeric types in haskell. But all
-- operations assume properly related widths.
module Seri.Bit (
    Bit(), bv_make, bv_width, bv_value, bv_signed_value,
    bv_concat, bv_extract, bv_truncate,
    bv_zero_extend, bv_sign_extend,
    bv_shl, bv_lshr,
    ) where

import Data.Typeable
import Data.Bits

data Bit = Bit {
    bv_width :: Integer,
    
    -- | The value is stored as the smallest positive integer representing the
    -- bits of the bit vector.
    bv_value :: Integer
} deriving (Typeable)

instance Show Bit where
    show (Bit w v) = show w ++ "'d" ++ show v

-- | Construct a bit vector of the given width and value.
-- Truncates the value as appropriate to fit in the given width.
bv_make :: Integer -> Integer -> Bit
bv_make width value = Bit width (mkMask width .&. value)

-- Mask for the given width number
mkMask :: Integer -> Integer
mkMask i = (shiftL 1 (fromInteger i)) - 1

instance Eq Bit where
    (==) (Bit _ a) (Bit _ b) = a == b

instance Ord Bit where
    compare (Bit _ a) (Bit _ b) = compare a b

instance Num Bit where
    (+) (Bit w a) (Bit _ b) = bv_make w (a+b)
    (*) (Bit w a) (Bit _ b) = bv_make w (a*b)
    (-) (Bit w a) (Bit _ b) = bv_make w (a-b)
    fromInteger i = error $ "fromInteger for Bit with unspecified width"
    abs = error "todo: abs Bit"
    signum = error "todo: signum Bit"
    
instance Bits Bit where
    (.&.) (Bit w a) (Bit _ b) = bv_make w (a .&. b)
    (.|.) (Bit w a) (Bit _ b) = bv_make w (a .|. b)
    xor (Bit w a) (Bit _ b) = bv_make w (a `xor` b)
    complement (Bit w a) = bv_make w (complement a)
    shift (Bit w a) i = bv_make w (shift a i)
    rotate = error $ "TODO: Bit rotate"
    bitSize (Bit w _) = fromInteger w
    isSigned _ = False

bv_zero_extend :: Integer -> Bit -> Bit
bv_zero_extend n (Bit w a) = bv_make (w+n) a

bv_sign_extend :: Integer -> Bit -> Bit
bv_sign_extend n b@(Bit w a) = bv_make (w+n) (bv_signed_value b)

bv_signed_value :: Bit -> Integer
bv_signed_value (Bit w a) =
  if (testBit a (fromIntegral w - 1))
    then a - (1 `shiftL` fromIntegral w)
    else a

bv_concat :: Bit -> Bit -> Bit
bv_concat a b =
    let aw = bv_width a
        bw = bv_width b
        a' = bv_zero_extend bw a
        b' = bv_zero_extend aw b
    in (a' `shiftL` fromIntegral bw) .|. b'

-- bv_extract i j x
-- Extract bits i downto j from the bit vector.
-- Example: bv_extract 3 1 'b0010110
-- Is: 'b011
bv_extract :: Integer -> Integer -> Bit -> Bit
bv_extract i j b = bv_truncate (i - j + 1) (b `shiftR` fromIntegral j)

-- Truncate a bit vector to the given number of bits.
bv_truncate :: Integer -> Bit -> Bit
bv_truncate w (Bit _ a) = bv_make w a

bv_shl :: Bit -> Bit -> Bit
bv_shl (Bit w a) (Bit _ b) = bv_make w (a `shiftL` fromInteger b)

bv_lshr :: Bit -> Bit -> Bit
bv_lshr (Bit w a) (Bit _ b) = bv_make w (a `shiftR` fromInteger b)

