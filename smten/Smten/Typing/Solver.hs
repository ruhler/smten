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

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Smten.Typing.Solver (solve) where

import Debug.Trace

import Control.Monad.State

import qualified Data.Map as Map

import Smten.Name
import Smten.Type
import Smten.Ppr

-- | Solve a type constraint system.
--    The solution set is returned. Unsolveable constraints are ignored.
solve :: [(Type, Type)] -> Map.Map Name Type
solve xs = finalize $ evalState finish (xs, Map.empty)

type Solver = State ([(Type, Type)], Map.Map Name Type)

finish :: Solver (Map.Map Name Type)
finish = do
    (sys, sol) <- get
    case sys of
        [] -> return sol
        ((a, b):xs) -> do
            put (xs, sol)
            let l = \n -> Map.lookup n sol
            single (fixassign l a, fixassign l b)
            finish

-- Apply a single constraint
--  Updates the current system and solution.
--
-- Note: we ignore unsolvable constraints rather than throw an error here so
-- the type checker can give more useful error messages.
single :: (Type, Type) -> Solver ()
single (a, b)
  | AppT a1 a2 <- a, AppT b1 b2 <- b = do
      (sys, sol) <- get
      put ((a1,b1) : (a2,b2) : sys, sol)
  | VarT na _ <- a, istarget na = update na b
  | VarT nb _ <- b, istarget nb = update nb a
  | otherwise = return ()

-- Update the solution with var n = t
update :: Name -> Type -> Solver ()
update n t
 | hasVarT n t = return ()  -- avoid recursive definitions
 | otherwise = do
    (sys, sol) <- get
    put (sys, Map.insert n t sol)

-- Return true if the given VarT name appears anywhere in the given type.
hasVarT :: Name -> Type -> Bool
hasVarT nm t =
  case t of
    ConT {} -> False
    AppT a b -> hasVarT nm a || hasVarT nm b
    OpT _ a b -> hasVarT nm a || hasVarT nm b
    VarT n _ -> nm == n
    NumT {} -> False
    UnknownT -> False

-- | Apply assignments in the given table to the given type until a fixed point
-- is reached.
fixassign :: (Name -> Maybe Type) -> Type -> Type
fixassign l t =
  let t' = assignl l t
  in if t == t'
        then t
        else fixassign l t'

-- | Given the solution, finalize it so each value is fully simplified.
finalize :: Map.Map Name Type -> Map.Map Name Type
finalize m =
  let m' = Map.map (assignl (flip Map.lookup m)) m
  in if m == m'
        then m
        else finalize m'

instance Ppr [(Type, Type)] where
    ppr ts =
       let pprt (a, b) = ppr a <> text ":" <+> ppr b
       in vcat (map pprt ts)
    
instance Ppr (Map.Map Name Type) where
    ppr m = 
        let pprt :: (Name, Type) -> Doc
            pprt (a, b) = ppr a <> text ":" <+> ppr b
        in vcat (map pprt (Map.assocs m))

-- There are two kinds of type variables in our constraints:
-- 1. User type variables.
--      We don't need to solve for these, because they are already in
--      scope.
-- 2. Target type variables.
--      We need to solve for these in terms of the user type variables.
-- Currently the type inference procedure puts "~" at the beginning of
-- each target type variable.
--
-- TODO: this is relies on assumptions about names of target type
--  variables in other modules. That's terribly messy and hackish. We
--  should instead make it explicit which variables we are solving for
--  and which we can take as given.
istarget :: Name -> Bool
istarget n = nhead n == '~'

