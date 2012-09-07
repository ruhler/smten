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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seri.SMT.Run (
    run
    ) where

import Debug.Trace

import Data.Maybe

import System.IO

import Seri.Failable
import Seri.Lambda hiding (free, query)
import Seri.SMT.Query
import Seri.Target.Elaborate

run :: Exp -> Query Exp
run e = do
    env <- envQ
    case elaborate WHNF env e of
        (AppE (VarE (Sig n _)) arg) | n == name "Seri.SMT.SMT.query" -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' -> do
                    return $ AppE (ConE (Sig (name "Satisfiable") (AppT (ConT (name "Answer")) (typeof arg)))) arg'
                Unsatisfiable -> return $ ConE (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ ConE (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        (VarE (Sig n (AppT _ t))) | n == name "Seri.SMT.SMT.free" -> free t
        (AppE (VarE (Sig n _)) p) | n == name "Seri.SMT.SMT.assert" -> do
            assert p
            return (ConE (Sig (name "()") (ConT (name "()"))))
        (AppE (VarE (Sig n _)) q) | n == name "Seri.SMT.SMT.queryS" -> do
            queryS $ do
                x <- run q
                run $ AppE (VarE (Sig (name "Seri.SMT.SMT.query") UnknownT)) x
        (AppE (VarE (Sig n _)) x) | n == name "Seri.SMT.SMT.return_query" -> return x
        (AppE (AppE (VarE (Sig n _)) x) f) | n == name "Seri.SMT.SMT.bind_query" -> do
          result <- run x
          run (AppE f result)
        (AppE (AppE (VarE (Sig n _)) x) y) | n == name "Seri.SMT.SMT.nobind_query" -> do
          run x
          run y
        x -> error $ "unknown Query: " ++ pretty x

