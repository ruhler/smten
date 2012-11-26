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

module Seri.Typing.Solver (solve) where

import Debug.Trace

import Control.Monad.State

import qualified Data.Map as Map

import Seri.Failable
import Seri.Name
import Seri.Type
import Seri.Ppr

-- | Solve a type constraint system.
-- Here's how we solve it:
--    We define an order on Types based on how well known they are. So
--    IntegerT, ConT, etc... are very well known. VarT less so, and we say
--    VarT 4 is less known than VarT 1, for instance.
--
--    For any constraint of the form X = Y, we use that to replace every
--    occurence of the less well known type with the more well known type. For
--    example, say Y is less well known. Every occurence of Y in all the
--    constraints is replaced with X, and we add Y = X to the solution set.
--
--    The claim is, after going through each constraint, we are left with
--    the best known definitions of each lesser known type we can find.
--
--    The solution set is returned.
--
--  Fails if the constraints are inconsistent.
solve :: [(Type, Type)] -> Failable (Map.Map Name Type)
solve xs = return . finalize $ evalState finish (xs, Map.empty)

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

-- Solve a single constraint
--  Updates the current system and solution.
--
-- Note: we ignore unsolvable constraints rather than throw an error here so
-- the type checker can give more useful error messages.
single :: (Type, Type) -> Solver ()
single (a, b) | unsolvable (a, b) = return ()
single (x, y) | x == y = return ()
single (AppT a b, AppT c d) = do
    (sys, sol) <- get
    put ((a,c) : (b,d) : sys, sol)
single (NumT (AppNT _ _ _), NumT (AppNT _ _ _)) = return ()
single (a, b) | b `lessknown` a = single (b, a)
single (VarT nm, b) | hasVarT nm b = return ()
single (VarT nm, b) = do
    (sys, sol) <- get
    put (sys, Map.insert nm b sol)
single (NumT (VarNT nm), b) = do
    (sys, sol) <- get
    put (sys, Map.insert nm b sol)
single (a, b) = error $ "single: unexpected assignment: " ++ pretty a ++ ": " ++ pretty b

-- Return true if the given VarT name appears anywhere in the given type.
hasVarT :: Name -> Type -> Bool
hasVarT nm t =
  case t of
    ConT {} -> False
    AppT a b -> hasVarT nm a || hasVarT nm b
    VarT n -> nm == n
    NumT {} -> False
    UnknownT -> False

solvable :: (Type, Type) -> Bool
solvable (VarT {}, _) = True
solvable (_, VarT {}) = True
solvable (ConT a, ConT b) = a == b
solvable (AppT {}, AppT {}) = True
solvable (NumT (VarNT {}), NumT {}) = True
solvable (NumT {}, NumT (VarNT {})) = True
solvable (NumT (ConNT a), NumT (ConNT b)) = a == b
solvable (NumT (AppNT a _ _), NumT (AppNT b _ _)) = a == b
solvable _ = False

unsolvable :: (Type, Type) -> Bool
unsolvable = not . solvable

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

lessknown :: Type -> Type -> Bool
lessknown (VarT a) (VarT b) = a > b
lessknown (VarT _) _ = True
lessknown (NumT (VarNT a)) (NumT (VarNT b)) = a > b
lessknown (NumT (VarNT _)) (NumT _) = True
lessknown a b = False

instance Ppr [(Type, Type)] where
    ppr ts =
       let pprt (a, b) = ppr a <> text ":" <+> ppr b
       in vcat (map pprt ts)
    
instance Ppr (Map.Map Name Type) where
    ppr m = 
        let pprt :: (Name, Type) -> Doc
            pprt (a, b) = ppr a <> text ":" <+> ppr b
        in vcat (map pprt (Map.assocs m))

