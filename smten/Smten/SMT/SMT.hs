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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Smten.SMT.SMT (
    Used(..),
    Symbolic,
    prim_free, assert, used,
    predicated, de_symbolicEH,
    Realize(), runSMT, runSymbolic,
    SMT, query, query_Used, query_Sat, nest, use, realize,
    ) where

import Data.Functor
import Data.Unique
import Data.Typeable

import Control.Monad.State

import qualified Smten.SMT.Solver as SMT

import Smten.Bit
import Smten.Sig
import Smten.Lit
import Smten.Type
import Smten.ExpH
import Smten.Ppr hiding (nest)

newtype Realize a = Realize {
    runRealize :: SMT a
} deriving (Functor, Monad)

data QS = QS {
    -- The current context ID stack.
    -- This is used to keep track of which context 'used' may be called.
    qs_ctx :: Contexts,

    qs_solver :: SMT.Solver,

    -- The current assertion predicate.
    -- Given predicate 'p', whenever the user says (assert q), we predicate
    -- the assertion by actually asserting (p `implies` q).
    qs_pred :: ExpH,

    -- List of free variables in scope.
    qs_freevars :: [Sig],

    -- Cache of free variable values.
    -- These are only valid between the time 'check' returns satisfiable and
    -- updates them, to the time a new assertion is made, or the SMT context
    -- is changed in some other way. After that, these are meaningless.
    qs_freevals :: [ExpH],

    -- A single boolean predicate representing the conjunction of all
    -- assertions made so far.
    qs_asserts :: ExpH
}

newtype SMT a = SMT (StateT QS IO a)
    deriving (Functor, Monad, MonadIO, Typeable)

deriving instance MonadState QS SMT

check :: SMT SMT.Result
check = {-# SCC "Check" #-} do
    res <- srun0 SMT.check
    case res of
        SMT.Satisfiable -> do
            -- Verify the assignment actual does satisfy the assertions to
            -- make sure there was no issue with abstraction of _|_.
            getmodel 
            asserts <- gets qs_asserts
            v <- runRealize (realize asserts)
            case force v of
                _ | Just True <- de_boolEH v -> return ()
                ErrorEH s -> error $ "smten user error: " ++ s
                _ -> error "SMTEN INTERNAL ERROR: SMT solver lied?"
        SMT.Unsatisfiable -> return ()
    return res

mkQS :: SMT.Solver -> IO QS
mkQS s = do
    ctx0 <- newUnique
    return $ QS {
        qs_ctx = [ctx0],
        qs_solver = s,
        qs_pred = trueEH,
        qs_freevars = [],
        qs_freevals = [],
        qs_asserts = trueEH
    }

-- | Evaluate a query.
runSMT :: SMT.Solver -> SMT a -> IO a
runSMT s (SMT q) = do
    qs <- mkQS s
    evalStateT q qs

runSymbolic :: SMT.Solver -> Symbolic (Realize a) -> IO (Maybe a)
runSymbolic s q = runSMT s (query q)

-- | Given a free variable name and corresponding smten type, return the
-- assignment for that free variable from the smt model.
--
-- Assumes:
--   Integers, Bools, and Bit vectors are implemented directly using the
--   corresponding smt primitives. (Should I not be assuming this?)
assignment :: Sig -> SMT ExpH
assignment s@(Sig nm t)
  | t == boolT = boolEH <$> srun1 SMT.getBoolValue nm
  | t == integerT = integerEH <$> srun1 SMT.getIntegerValue nm
  | Just w <- de_bitT t = do
    bval <- srun2 SMT.getBitVectorValue w nm
    return (bitEH t (bv_make w bval))
  | otherwise = error $
    "SMTEN INTERNAL ERROR: unexpected type for prim free var: " ++ pretty s

query_Used :: (Used (Realize a)) -> SMT (Maybe a)
query_Used (Used ctx rx) = do
    ctxs <- gets qs_ctx
    if (ctx `notElem` ctxs)
        then error "query_Used: invalid context for Used"
        else return ()
    res <- query_Sat
    if res 
        then Just <$> runRealize rx
        else return Nothing
        
query :: Symbolic (Realize a) -> SMT (Maybe a)
query sr = nest (use sr >>= query_Used)

-- Ask if the existing context is satisfied or not.
query_Sat :: SMT Bool
query_Sat = do
    res <- check
    return $ res == SMT.Satisfiable
  
-- | Assert the given smten boolean expression.
mkassert :: ExpH -> SMT ()
mkassert p = do
  pred <- gets qs_pred
  let p_predicated = impliesEH pred p
  srun1 SMT.assert p_predicated
  modify $ \qs -> qs { qs_asserts = andEH (qs_asserts qs) p_predicated }

use :: Symbolic a -> SMT (Used a)
use s = do
    v <- symbolic_smt s
    ctx <- gets qs_ctx
    return (Used (head ctx) v)

srun0 :: (SMT.Solver -> IO a) -> SMT a
srun0 f = do
    s <- gets qs_solver
    liftIO $ f s

srun1 :: (SMT.Solver -> a -> IO b) -> a -> SMT b
srun1 f x = do
    s <- gets qs_solver
    liftIO $ f s x

srun2 :: (SMT.Solver -> a -> b -> IO c) -> a -> b -> SMT c
srun2 f x y = do
    s <- gets qs_solver
    liftIO $ f s x y
    
-- | Run the given query in its own scope and return the result.
nest :: SMT a -> SMT a
nest q = do
  nctx <- liftIO newUnique
  qs <- get
  put $! qs { qs_ctx = nctx : qs_ctx qs }
  srun0 SMT.push
  v <- q
  srun0 SMT.pop
  put qs
  return v

-- Read the current model from the SMT solver.
-- Assumes the state is satisfiable.
getmodel :: SMT ()
getmodel = do
    freevars <- gets qs_freevars
    freevals <- mapM assignment freevars
    modify $ \qs -> qs { qs_freevals = freevals }

-- | Update the free variables in the given expression based on the current
-- model.
-- Assumes the state is satisfiable and the model has already been read.
realize :: ExpH -> Realize ExpH
realize e = Realize $ do
    freevars <- gets qs_freevars
    fvs <- gets qs_freevals
    let freemap = zip [n | Sig n _ <- freevars] fvs
        g e | VarEH nm <- force e = lookup nm freemap
            | otherwise = Nothing
    return $ if null freemap then e else transform g e


type Context = Unique
type Contexts = [Unique]

data Used a = Used Context a
    deriving (Typeable)

instance Functor Used where
    fmap f (Used ctx a) = Used ctx (f a)

newtype Symbolic a = Symbolic {
    symbolic_smt :: SMT a
} deriving (Functor, Monad, MonadIO, Typeable)

deriving instance MonadState QS Symbolic

-- | Assert the given predicate.
assert :: ExpH -> Symbolic ()
assert p = Symbolic (mkassert p)

-- | Read the value of a Used.
used :: Used a -> Symbolic a
used (Used ctx v) = do
    ctxs <- gets qs_ctx
    if (ctx `elem` ctxs)
       then return $ v
       else error "used of ref in invalid context"

-- | Allocate a primitive free variable of the given type.
-- The underlying SMT solver must support this type for this to work.
prim_free :: Type -> Symbolic ExpH
prim_free t = Symbolic $ do
    nm <- srun1 SMT.fresh t
    let s = Sig nm t
    modify $ \qs -> qs { qs_freevars = s : qs_freevars qs }
    return $ varEH t nm

-- | Predicate the symbolic computation on the given smten Bool.
-- All assertions will only apply when the predicate is satisfied, otherwise
-- the assertions become vacuous.
predicated :: ExpH -> Symbolic a -> Symbolic a
predicated p s = do
    pred <- gets qs_pred
    modify $ \qs -> qs { qs_pred = andEH pred p }
    v <- s
    modify $ \qs -> qs { qs_pred = pred }
    return v

-- Convert an ExpH of smten type (Symbolic a) to it's corresponding haskell
-- Symbolic.
--
-- This assumes you are passing an expression of seri type (Symbolic a).
-- It will always succeed, because it automatically converts symbolic
-- (Symbolic a) into concrete (Symbolic a)
de_symbolicEH :: ExpH -> Symbolic ExpH
de_symbolicEH e
 | Just l <- de_litEH e, Just s <- de_dynamicL l = s
 | IfEH x y n <- force e =
    let py = x
        pn = ifEH boolT x falseEH trueEH

        ys = de_symbolicEH y
        ns = de_symbolicEH n
    in do
        yr <- predicated py ys
        nr <- predicated pn ns
        return $ ifEH (typeof e) x yr nr
 | ErrorEH s <- force e = do
    -- TODO: This is messy. I'm sure there is a way to clean it up if I
    -- actually spend time to think about it.
    let Just (_, t) = de_appT (typeof e)
    pred <- gets qs_pred
    modify $ \qs -> qs { qs_pred = trueEH }
    sat <- Symbolic $ query (assert pred >> return (return ()))
    modify $ \qs -> qs { qs_pred = pred }
    case sat of
        Just () -> error s
        Nothing -> return (errorEH t s)
 | otherwise = error $ "SMTEN INTERNAL ERROR: unexpected argument to de_symbolicEH: " ++ pretty e

