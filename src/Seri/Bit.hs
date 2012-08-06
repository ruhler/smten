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

-- | Bit vector library. Widths are specified dynamically instead of enforced
-- statically to avoid complications with numeric types in haskell. But all
-- operations assume properly related widths.
module Seri.Bit (Bit(), bv_width, bv_value, mkBit) where

import Data.Bits

data Bit = Bit {
    bv_width :: Integer,
    bv_value :: Integer
} deriving (Show)

-- | Construct a bit vector of the given width and value.
-- Truncates the value as appropriate to fit in the given width.
mkBit :: Integer -> Integer -> Bit
mkBit width value = Bit width (mkMask width .&. value)

-- Mask for the given width number
mkMask :: Integer -> Integer
mkMask i = (shiftL 1 (fromInteger i)) - 1

instance Eq Bit where
    (==) (Bit _ a) (Bit _ b) = a == b

instance Num Bit where
    (+) (Bit w a) (Bit _ b) = mkBit w (a+b)
    (*) (Bit w a) (Bit _ b) = mkBit w (a*b)
    (-) (Bit w a) (Bit _ b) = mkBit w (a-b)
    fromInteger i = error $ "fromInteger for Bit with unspecified width"
    abs = error "todo: abs Bit"
    signum = error "todo: signum Bit"
    
instance Bits Bit where
    (.&.) (Bit w a) (Bit _ b) = mkBit w (a .&. b)
    (.|.) (Bit w a) (Bit _ b) = mkBit w (a .|. b)
    xor (Bit w a) (Bit _ b) = mkBit w (a `xor` b)
    complement (Bit w a) = mkBit w (complement a)
    shift (Bit w a) i = mkBit w (shift a i)
    rotate = error $ "TODO: Bit rotate"
    bitSize (Bit w _) = fromInteger w
    isSigned _ = False

