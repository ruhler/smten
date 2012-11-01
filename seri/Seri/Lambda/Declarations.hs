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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Utilities for working with seri declarations.
module Seri.Lambda.Declarations (
    minimize, sort,
    ) where

import Data.List(nub, partition)
import Data.Maybe

import Seri.Failable
import Seri.Lambda.Env
import Seri.Lambda.Generics
import Seri.Lambda.IR


data Declarations = Declarations Env

instance Querier Declarations [Dec] where
    q_Exp (Declarations env) (VarE s@(Sig n _)) =
       case (attemptM $ lookupVarInfo env s) of
          Just Primitive -> attemptM $ lookupPrimD env n
          Just Declared -> attemptM $ lookupValD env n
          Just (Instance cls@(Class ni ts)) ->
              catMaybes [attemptM $ lookupClassD env ni,
                           attemptM $ lookupInstD env cls]
          _ -> []
    q_Exp _ e = []

    q_Type (Declarations env) (ConT n) = attemptM $ lookupDataD env n
    q_Type _ t = []

    q_Class (Declarations env) cls@(Class n ts) = catMaybes [
          attemptM $ lookupClassD env n,
          attemptM $ lookupInstD env cls]

-- declarations env x
-- Return the set of declarations in the given environment a thing depends on.
declarations :: (Queriable a [Dec]) => Env -> a -> [Dec]
declarations env x = nub $ query (Declarations env) x

-- | Minimize an environment.
-- Remove any declarations in the environment not needed by the object in the
-- environment.
minimize :: (Queriable a [Dec]) => Env -> a -> [Dec]
minimize m x =
  let alldecls :: [Dec] -> [Dec]
      alldecls d =
        let ds = declarations m d
            dds = d ++ ds
        in if (length d == length dds)
            then d
            else alldecls dds
  in nub $ alldecls (declarations m x)

-- | sort ds
-- Perform a topological sort of declarations ds by dependency.
--   returns (sorted, mutual)
--  sorted - the list of sorted declarations. Declarations earlier in the list
--           do not depend on declarations later in the list.
--  mutual - a list of the remaining mutually dependent declarations from ds.
--
-- TODO: should this take an Env as input too?
sort :: [Dec] -> ([Dec], [Dec])
sort ds = 
  let dependencies :: [(Dec, [Dec])]
      dependencies = [(d, declarations (mkEnv ds) d) | d <- ds]

      sorte :: [Dec] -> [(Dec, [Dec])] -> ([Dec], [Dec])
      sorte sorted unsorted = 
        let indep :: (Dec, [Dec]) -> Bool
            indep (d, deps) = and [dp `elem` sorted | dp <- deps]
        in case partition indep unsorted of
            ([], deps) -> (sorted, map fst unsorted)
            (indeps, deps) -> sorte (sorted ++ (map fst indeps)) deps
  in sorte [] dependencies

