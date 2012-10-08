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

module Seri.Lambda.Utils (
    free, free',
    Uses(..), uses
    ) where

import Data.List(nub)
import Data.Monoid

import Seri.Lambda.IR
import Seri.Lambda.Types


-- | Return a list of the free variables in the given expression.
free :: Exp -> [Sig]
free =
  let free' :: [Name] -> Exp -> [Sig]
      free' _ (LitE {}) = []
      free' _ (ConE {}) = []
      free' bound (VarE (Sig n _)) | n `elem` bound = []
      free' _ (VarE s) = [s]
      free' bound (AppE a bs) = nub $ free' bound a ++ concat (map (free' bound) bs)
      free' bound (LaceE ms) =
        let freem :: Match -> [Sig]
            freem (Match ps b) = free' (map (\(Sig n _) -> n) (concat (map bindingsP ps)) ++ bound) b
        in nub $ concat (map freem ms)
  in free' []

free' :: Exp -> [Name]
free' e = [n | Sig n _ <- free e]

data Uses = NoUse | SingleUse | MultiUse
    deriving(Eq, Show)

instance Num Uses where
  fromInteger 0 = NoUse
  fromInteger 1 = SingleUse
  fromInteger _ = MultiUse

  (+) NoUse x = x
  (+) SingleUse NoUse = SingleUse
  (+) SingleUse _ = MultiUse
  (+) MultiUse _ = MultiUse

  (*) = error "* Uses"
  abs = error "abs Uses"
  signum = error "signum Uses"

-- Return the number of times a variable is used in the given expression.
uses :: (Num n) => Name -> Exp -> n
uses n =
  let uses' :: (Num n) => Exp -> n
      uses' (LitE {}) = 0
      uses' (ConE {}) = 0  
      uses' (VarE (Sig nm _)) | nm == n = 1
      uses' (VarE {}) = 0
      uses' (AppE a bs) = uses' a + sum (map uses' bs)
      uses' (LaceE ms) = 
        let usesm :: (Num n) => Match -> n
            usesm (Match ps _) | n `elem` concatMap bindingsP' ps = 0
            usesm (Match _ b) = uses' b
        in sum (map usesm ms)
  in uses'
      
