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

module Seri.Target.Inline (inline) where

import Seri.Failable
import Seri.Lambda

-- | Inline all variables in the given expression to the given depth.
inline :: Integer -> Env -> Exp -> Exp
inline = inline' []

-- | Same as inline, but don't inline any variables in the given bound set.
inline' :: [Name] -> Integer -> Env -> Exp -> Exp
inline' _ 0 _ e = e
inline' bound depth env e =
  let inme = inline' bound depth env
  in case e of
        LitE {} -> e
        CaseE a ms ->
           let imatch (Match p b) =
                let nbound = bindingsP' p
                in Match p (inline' (nbound ++ bound) depth env b)
           in CaseE (inme a) (map imatch ms)
        AppE a b -> AppE (inme a) (inme b)
        LamE (Sig n t) b -> LamE (Sig n t) (inline' (n:bound) depth env b)
        ConE {} -> e
        (VarE s@(Sig n ct)) | not (n `elem` bound) ->
            case attemptM $ lookupVar env s of
                Just (pt, ve) -> inline' bound (depth-1) env (assign (assignments pt ct) ve)
                Nothing -> e
        VarE {} -> e

