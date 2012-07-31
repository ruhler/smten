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

module Seri.Lambda.TypeSolver (solve) where

import Control.Monad.State

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Ppr

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
solve :: [(Type, Type)] -> Failable [(Type, Type)]
solve xs = return . finalize $ evalState finish (xs, [])

type Solver = State ([(Type, Type)], [(Type, Type)])

finish :: Solver [(Type, Type)]
finish = do
    (sys, sol) <- get
    case sys of
        [] -> return sol
        (x:xs) -> do
            put (xs, sol)
            single x
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
single (NumT (AppNT _ a b), NumT (AppNT _ c d)) = do
    (sys, sol) <- get
    put ((NumT a, NumT c) : (NumT b, NumT d) : sys, sol)
single (a, b) | b `lessknown` a = single (b, a)
single (a, b) = do
    (sys, sol) <- get
    let sys' = map (tpreplace a b) sys
    put (sys', (a,b):sol)

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

-- | Given the solution, finalize it so each value is fully simplified.
finalize :: [(Type, Type)] -> [(Type, Type)]
finalize [] = []
finalize ((a, b):ts) = 
  let nts = map (tpreplace a b) ts
  in (a, b) : finalize nts

tpreplace :: Type -> Type -> (Type, Type) -> (Type, Type)
tpreplace k v (a, b) = (treplace k v a, treplace k v b)

-- treplace k v x
-- Replace every occurence of type k in x with type v.
treplace :: Type -> Type -> Type -> Type
treplace k v x | k == x = v
treplace (NumT k) (NumT v) (NumT x) = NumT (tnreplace k v x)
treplace k v (AppT a b) = AppT (treplace k v a) (treplace k v b)
treplace _ _ x = x 

tnreplace :: NType -> NType -> NType -> NType
tnreplace k v x | k == x = v
tnreplace k v (AppNT o a b) = AppNT o (tnreplace k v a) (tnreplace k v b)
tnreplace _ _ x = x

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
    

