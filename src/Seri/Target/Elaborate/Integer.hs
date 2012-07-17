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

-- | Rules for reducing integer primitives.
module Seri.Target.Elaborate.Integer (integerR) where

import Seri.Lambda
import Seri.Target.Elaborate.Elaborate

isibop :: String -> Exp -> Bool
isibop op (AppE (AppE (VarE (Sig nm _)) (LitE (IntegerL _))) (LitE (IntegerL _))) = nm == op
isibop _ _ = False

apibop :: (Integer -> Integer -> Exp) -> Exp -> Exp 
apibop f (AppE (AppE (VarE (Sig _ _)) (LitE (IntegerL a))) (LitE (IntegerL b))) = f a b
apibop f e = error $ "not an integer binary operation: " ++ pretty e

integerR :: (Monad m) => Rule m
integerR = Rule $ \gr env e ->
    case e of 
        _ | isibop "__prim_add_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a+b)) e
        _ | isibop "__prim_sub_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a-b)) e
        _ | isibop "__prim_mul_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a*b)) e
        _ | isibop "<" e ->
            return . Just $ apibop (\a b -> boolE (a < b)) e
        _ | isibop ">" e ->
            return . Just $ apibop (\a b -> boolE (a > b)) e
        _ | isibop "__prim_eq_Integer" e ->
            return . Just $ apibop (\a b -> boolE (a == b)) e
        _ -> return Nothing

