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
    ) where

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
