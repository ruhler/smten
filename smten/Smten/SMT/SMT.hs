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

import Debug.Trace

import Data.Functor
import Data.IORef
import Data.Unique
import Data.Typeable

import System.IO

import Control.Monad.State
import qualified Data.HashMap as Map

import qualified Smten.SMT.Syntax as SMT
import qualified Smten.SMT.Solver as SMT

import Smten.Bit
import Smten.Failable
import Smten.Name hiding (smtN)
import Smten.Sig
import Smten.Lit
import Smten.Type
import Smten.ExpH
import Smten.Dec hiding (Context)
import Smten.Ppr hiding (nest)
import Smten.SMT.Translate

newtype Realize a = Realize {
    runRealize :: SMT a
} deriving (Functor, Monad)

data QS = QS {
    -- The current context ID stack.
    -- This is used to keep track of which context 'used' may be called.
    qs_ctx :: Contexts,

    qs_solver :: SMT.Solver,

    -- ID to use for the next primitive free variable.
    qs_freeid :: Integer,

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

sendCmds :: [SMT.Command] -> SMT.Solver -> IO ()
sendCmds cmds solver = mapM_ (SMT.run solver) cmds

runCmds :: [SMT.Command] -> SMT ()
runCmds cmds = {-# SCC "RunCmds" #-} do
    solver <- gets qs_solver
    liftIO $ sendCmds cmds solver

check :: SMT SMT.Result
check = {-# SCC "Check" #-} do
    solver <- gets qs_solver
    res <- {-# SCC "SMTCheck" #-} liftIO $ SMT.check solver
    case res of
        SMT.Satisfiable -> do
            -- Verify the assignment actual does satisfy the assertions to
            -- make sure there was no issue with abstraction of _|_.
            getmodel 
            asserts <- gets qs_asserts
            v <- runRealize (realize asserts)
            case {-# SCC "CheckEval" #-} force v of
                _ | Just True <- de_boolEH v -> return ()
                ErrorEH _ s -> error $ "smten user error: " ++ s
                _ -> error "SMTEN INTERNAL ERROR: SMT solver lied?"
        SMT.Unsatisfiable -> return ()
    return res

isPrimT :: Type -> Bool
isPrimT t
  | t == boolT = True
  | t == integerT = True
  | Just _ <- de_bitT t = True
  | otherwise = False
            
mkQS :: SMT.Solver -> IO QS
mkQS s = do
    ctx0 <- newUnique
    return $ QS {
        qs_ctx = [ctx0],
        qs_solver = s,
        qs_freeid = 1,
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
  | t == boolT = do
    solver <- gets qs_solver
    bval <- liftIO $ SMT.getBoolValue solver (smtN nm)
    return (boolEH bval)
  | t == integerT = do
    solver <- gets qs_solver
    ival <- liftIO $ SMT.getIntegerValue solver (smtN nm)
    return (integerEH ival)
  | Just w <- de_bitT t = do
    solver <- gets qs_solver
    bval <- liftIO $ SMT.getBitVectorValue solver w (smtN nm)
    return (bitEH (bv_make w bval))
  | otherwise = error $
    "SMTEN INTERNAL ERROR: unexpected type for prim free var: " ++ pretty s

query_Used :: (Used (Realize a)) -> SMT (Maybe a)
query_Used (Used ctx rx) = {-# SCC "QUERY_USED" #-} do
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

mkfree :: Sig -> SMT ()
mkfree s@(Sig nm t) = do
  modify $ \qs -> qs { qs_freevars = s : qs_freevars qs }
  runCmds [SMT.Declare (smtN nm) (smtT t)]

mkerr :: Type -> SMT ExpH
mkerr t = do
    fid <- gets qs_freeid
    let nm = name $ "err~" ++ show fid
        s = Sig nm t
    modify $ \qs -> qs { qs_freeid = fid+1 }
    runCmds [SMT.Declare (smtN nm) (smtT t)]
    return (varEH s)

-- | Assert the given smten boolean expression.
mkassert :: ExpH -> SMT ()
mkassert p = do
  pred <- gets qs_pred
  let p_predicated = impliesEH pred p
  p_abstracted <- abstract p_predicated
  runCmds [SMT.Assert $ {-# SCC "TRANSLATE" #-} smtE (fromExpH p_abstracted)]
  modify $ \qs -> qs { qs_asserts = andEH (qs_asserts qs) p_predicated }

-- Replace all explicit _|_ with VarEH.
abstract :: ExpH -> SMT ExpH
abstract x = {-# SCC "Abstract" #-} do
  cache <- liftIO $ newIORef Map.empty
  let use :: ExpH -> SMT ExpH
      use e =
        case force e of
            LitEH {} -> return e
            ConEH {} -> return e
            VarEH {} -> return e
            _ -> do
                m <- liftIO $ readIORef cache
                case Map.lookup (eid e) m of
                    Just v -> return v
                    Nothing -> do
                        v <- def e
                        liftIO $ modifyIORef' cache (Map.insert (eid e) v)
                        return v

      def :: ExpH -> SMT ExpH
      def e =
        case force e of
            PrimEH n t f xs -> exph . (PrimEH n t f) <$> mapM use xs
            IfEH t x y d -> do
                x' <- use x
                y' <- use y
                d' <- use d
                return $ exph (IfEH t x' y' d')
            ErrorEH t _ -> mkerr t
            LitEH {} -> return e
            ConEH {} -> return e
            VarEH {} -> return e
  def x
 
use :: Symbolic a -> SMT (Used a)
use s = {-# SCC "USE" #-} do
    v <- symbolic_smt s
    ctx <- gets qs_ctx
    return (Used (head ctx) v)
    
-- | Run the given query in its own scope and return the result.
nest :: SMT a -> SMT a
nest q = {-# SCC "NEST" #-} do
  nctx <- liftIO newUnique
  qs <- get
  put $! qs { qs_ctx = nctx : qs_ctx qs }
  runCmds [SMT.Push]
  v <- q
  runCmds [SMT.Pop]
  nfreeid <- gets qs_freeid
  put qs { qs_freeid = nfreeid }
  return v

-- Read the current model from the SMT solver.
-- Assumes the state is satisfiable.
getmodel :: SMT ()
getmodel = {-# SCC "GetModel" #-} do
    freevars <- gets qs_freevars
    freevals <- mapM assignment freevars
    modify $ \qs -> qs { qs_freevals = freevals }

-- | Update the free variables in the given expression based on the current
-- model.
-- Assumes the state is satisfiable and the model has already been read.
realize :: ExpH -> Realize ExpH
realize e = {-# SCC "REALIZE" #-} Realize $ do
    freevars <- gets qs_freevars
    fvs <- gets qs_freevals
    let freemap = zip [n | Sig n _ <- freevars] fvs
        g e | VarEH (Sig nm _) <- force e = lookup nm freemap
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
assert p = {-# SCC "ASSERT" #-} Symbolic (mkassert p)

-- | Read the value of a Used.
used :: Used a -> Symbolic a
used (Used ctx v) = {-# SCC "USED" #-} do
    ctxs <- gets qs_ctx
    if (ctx `elem` ctxs)
       then return $ v
       else error "used of ref in invalid context"

-- | Allocate a primitive free variable of the given type.
-- The underlying SMT solver must support this type for this to work.
prim_free :: Type -> Symbolic ExpH
prim_free t = {-# SCC "PRIM_FREE" #-} do
    fid <- gets qs_freeid
    let f = Sig (name $ "free~" ++ show fid) t
    Symbolic $ mkfree f
    modify $ \qs -> qs { qs_freeid = fid+1 }
    return (varEH f)

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
 | IfEH t x y n <- force e =
    let py = x
        pn = ifEH boolT x falseEH trueEH

        ys = de_symbolicEH y
        ns = de_symbolicEH n
    in do
        yr <- predicated py ys
        nr <- predicated pn ns
        return $ ifEH t x yr nr
 | ErrorEH symt s <- force e = do
    -- TODO: This is messy. I'm sure there is a way to clean it up if I
    -- actually spend time to think about it.
    let Just (_, t) = de_appT symt
    pred <- gets qs_pred
    modify $ \qs -> qs { qs_pred = trueEH }
    sat <- Symbolic $ query (assert pred >> return (return ()))
    modify $ \qs -> qs { qs_pred = pred }
    case sat of
        Just () -> error s
        Nothing -> return (errorEH t s)
 | otherwise = error $ "SMTEN INTERNAL ERROR: unexpected argument to de_symbolicEH: " ++ pretty e

