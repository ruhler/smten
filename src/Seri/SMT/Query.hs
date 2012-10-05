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

module Seri.SMT.Query (
    Answer(..), Query(..), Realize(), 
    run,
    ) where

import Seri.Target.Elaborate
import Seri.Lambda hiding (free, query)
import Seri.SMT.Realize

data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

class (Functor q, Monad q) => Query q where
    -- | Check if the current assertions are satisfiable. If so, runs the given
    -- realize computation and returns that as the body of the Answer.
    query :: Realize q a -> q (Answer a)

    -- | Allocate a free expression of the given type.
    free :: Type -> q Exp

    -- | Assert the given seri boolean expression.
    assert :: Exp -> q ()

    -- | Run the given query in its own scope and return the result.
    -- Note: it's possible to leak free variables with this function.
    -- You should not return anything from the first query which could contain a
    -- free variable, otherwise who knows what will happen.
    queryS :: q a -> q a

    -- | Update the free variables in the given expression based on the current
    -- yices model.
    realize :: Exp -> Realize q Exp

    -- | Return the environment the query is running under.
    envQ :: q Env
    envR :: Realize q Env


-- | Given a Seri expression of type Query a,
-- returns the Seri expression of type a which results from running the query.
run :: (Query q) => Exp -> q Exp
run e = do
    env <- envQ
    case elabwhnf env e of
        (AppE (VarE (Sig n _)) [arg]) | n == name "Seri.SMT.SMT.query" -> do
            res <- query (realize arg)
            case res of 
                Satisfiable arg' -> do
                    return $ AppE (ConE (Sig (name "Satisfiable") (AppT (ConT (name "Answer")) (typeof arg)))) [arg']
                Unsatisfiable -> return $ ConE (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ ConE (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        (VarE (Sig n (AppT _ t))) | n == name "Seri.SMT.SMT.free" -> free t
        (AppE (VarE (Sig n _)) [p]) | n == name "Seri.SMT.SMT.assert" -> do
            assert p
            return (ConE (Sig (name "()") (ConT (name "()"))))
        (AppE (VarE (Sig n _)) [q]) | n == name "Seri.SMT.SMT.queryS" -> queryS $ run q
        (AppE (VarE (Sig n _)) [x]) | n == name "Seri.SMT.SMT.return_query" -> return x
        (AppE (VarE (Sig n _)) [x, f]) | n == name "Seri.SMT.SMT.bind_query" -> do
          result <- run x
          run (AppE f [result])
        (AppE (VarE (Sig n _)) [x, y]) | n == name "Seri.SMT.SMT.nobind_query" -> do
          run x
          run y
        x -> error $ "unknown Query: " ++ pretty x

