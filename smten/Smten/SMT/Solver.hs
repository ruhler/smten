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

-- | An SMT solver interface
module Smten.SMT.Solver (
    Result(..), Solver(..),
    ) where

import Smten.Name
import Smten.Type
import Smten.ExpH

data Result
    = Satisfiable
    | Unsatisfiable
    deriving (Eq, Show)

data Solver = Solver {
    push :: IO (),
    pop :: IO (),

    -- | Declare a fresh free variable with given type. Returns the name of
    -- the fresh variable.
    fresh :: Type -> IO Name,

    -- | Assert the given expression.
    assert :: ExpH -> IO (),

    -- | Run (check) and return the result.
    check :: IO Result,

    -- | Given the name of a free variable with integer type, return its
    -- value.
    getIntegerValue :: Name -> IO Integer,

    -- | Given the name of a free variable with bool type, return its
    -- value.
    getBoolValue :: Name -> IO Bool,

    -- | Given the width and name of a free variable with bit vector type,
    -- return its value as a positive integer.
    getBitVectorValue :: Integer -> Name -> IO Integer
}

