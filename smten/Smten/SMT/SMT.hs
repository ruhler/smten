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
    Realize(), RunOptions(..), runSMT, runSymbolic,
    SMT, query, query_Used, nest, use, realize,
    ) where

import Debug.Trace

import Data.Functor
import Data.Unique
import Data.Typeable

import System.IO

import Control.Monad.State

import qualified Smten.SMT.Syntax as SMT
import qualified Smten.SMT.Solver as SMT

import Smten.Bit
import Smten.Failable
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH
import Smten.Dec
import Smten.Ppr hiding (nest)
import Smten.SMT.IVP
import Smten.SMT.Symbolic
import Smten.SMT.Translate

newtype Realize a = Realize {
    runRealize :: SMT a
} deriving (Functor, Monad)

data QS = QS {
    qs_ctx :: Contexts,
    qs_solver :: SMT.Solver,
    qs_dh :: Maybe Handle,
    qs_freeid :: Integer,
    qs_qs :: Compilation,
    qs_freevars :: [Sig],
    qs_freevals :: Maybe [ExpH] -- ^ Cache of free variable values
}

newtype SMT a = SMT (StateT QS IO a)
    deriving (Functor, Monad, MonadIO, Typeable)

deriving instance MonadState QS SMT

sendCmds :: [SMT.Command] -> SMT.Solver -> Maybe Handle -> IO ()
sendCmds cmds solver Nothing = mapM_ (SMT.run solver) cmds
sendCmds cmds solver (Just dh) = do
    hPutStr dh (unlines (map (SMT.pretty solver) cmds))
    mapM_ (SMT.run solver) cmds

runCmds :: [SMT.Command] -> SMT ()
runCmds cmds = {-# SCC "RunCmds" #-} do
    solver <- gets qs_solver
    dh <- gets qs_dh
    liftIO $ sendCmds cmds solver dh

check :: SMT SMT.Result
check = {-# SCC "Check" #-} do
    solver <- gets qs_solver
    debug (SMT.pretty solver SMT.Check)
    res <- liftIO $ SMT.check solver
    debug $ "; check returned: " ++ show res
    modify $ \qs -> qs { qs_freevals = Nothing }
    return res

-- Output a line to the debug output.
debug :: String -> SMT ()
debug msg = do
    dh <- gets qs_dh
    case dh of
        Nothing -> return ()
        Just h -> liftIO $ hPutStrLn h msg

smtt :: Type -> SMT SMT.Type
smtt t = do
    qs <- gets qs_qs 
    let mkyt = do
          yt <- smtT t
          cmds <- smtD
          return (cmds, yt)
    ((cmds, yt), qs') <- liftIO . attemptIO $ runCompilation mkyt qs
    modify $ \s -> s { qs_qs = qs' }
    runCmds cmds
    return yt

smte' :: ExpH -> SMT ([SMT.Command], SMT.Expression)
smte' e = {-# SCC "SmtE" #-} do
    qs <- gets qs_qs 
    let se = {-# SCC "FROMEXPH" #-} fromExpH e
        mkye :: CompilationM ([SMT.Command], SMT.Expression)
        mkye = do
          ye <- smtE se
          cmds <- smtD
          return (cmds, ye)
    ((cmds, ye), qs') <- liftIO . attemptIO $ runCompilation mkye qs
    modify $ \s -> s { qs_qs = qs' }
    return (cmds, ye)

smte :: ExpH -> SMT SMT.Expression
smte e = do
    (cmds, ye) <- smte' e
    runCmds cmds
    return ye


isPrimT :: Type -> Bool
isPrimT t | t == boolT = True
isPrimT t | t == integerT = True
isPrimT (AppT (ConT n _) _) | n == name "Bit" = True
isPrimT _ = False

data RunOptions = RunOptions {
    -- | Optionally output debug info to the given file.
    ro_debugout :: Maybe FilePath,

    -- | The solver to use
    ro_solver :: SMT.Solver
}
            
mkQS :: RunOptions -> IO QS
mkQS opts = do
    dh <- case ro_debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)
    ctx0 <- newUnique

    return $ QS {
        qs_ctx = [ctx0],
        qs_solver = ro_solver opts,
        qs_dh = dh,
        qs_freeid = 1,
        qs_qs = compilation,
        qs_freevars = [],
        qs_freevals = Nothing
    }

-- | Evaluate a query.
runSMT :: RunOptions -> SMT a -> IO a
runSMT opts (SMT q) = do
    qs <- mkQS opts
    evalStateT q qs

runSymbolic :: RunOptions -> Symbolic (Realize a) -> IO (Maybe a)
runSymbolic opts q = runSMT opts (query q)

-- | Given a free variable name and corresponding smten type, return the value
-- of that free variable from the smt model.
--
-- Assumes:
--   Integers, Bools, and Bit vectors are implemented directly using the
--   corresponding smt primitives. (Should I not be assuming this?)
realizefree :: Sig -> SMT ExpH
realizefree (Sig nm t) | t == boolT = do
    solver <- gets qs_solver
    bval <- liftIO $ SMT.getBoolValue solver (smtN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show bval
    return (boolEH bval)
realizefree (Sig nm t) | t == integerT = do
    solver <- gets qs_solver
    ival <- liftIO $ SMT.getIntegerValue solver (smtN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show ival
    return (integerEH ival)
realizefree (Sig nm (AppT (ConT n _) wt)) | n == name "Bit" = do
    let w = nteval wt
    solver <- gets qs_solver
    bval <- liftIO $ SMT.getBitVectorValue solver w (smtN nm)
    debug $ "; " ++ pretty nm ++ " has value " ++ show bval
    return (bitEH (bv_make w bval))
realizefree (Sig _ t@(AppT (AppT (ConT n _) _) _)) | n == name "->"
  = return (error $ "TODO: realizefree type " ++ pretty t)
realizefree (Sig _ t)
  = return (error $ "unexpected realizefree type: " ++ pretty t)

query_Used :: (Used (Realize a)) -> SMT (Maybe a)
query_Used (Used ctx rx) = {-# SCC "QUERY_USED" #-} do
    ctxs <- gets qs_ctx
    if (ctx `notElem` ctxs)
        then error "query_Used: invalid context for Used"
        else return ()
    res <- check
    case res of
        SMT.Satisfiable -> Just <$> runRealize rx
        SMT.Unsatisfiable -> return Nothing
        _ -> error $ "Smten.SMT.SMT.query_Used: check failed"
        
query :: Symbolic (Realize a) -> SMT (Maybe a)
query sr = {-# SCC "QUERY" #-} nest (use sr >>= query_Used)

mkfree :: Sig -> SMT ()
mkfree s@(Sig nm t) | isPrimT t = do
  t' <- smtt t
  modify $ \qs -> qs { qs_freevars = s : qs_freevars qs }
  runCmds [SMT.Declare (smtN nm) t']
mkfree s = error $ "SMT.mkfree: unsupported type: " ++ pretty s

-- | Assert the given smten boolean expression.
mkassert :: ExpH -> SMT ()
mkassert p = {-# SCC "MKASSERT" #-}do
  yp <- smte p
  runCmds [SMT.Assert yp]

use :: Symbolic a -> SMT (Used a)
use s = {-# SCC "USE" #-} do
    ctx <- gets qs_ctx
    fid <- gets qs_freeid
    let (fid', frees, asserts, v) = {-# SCC "RUN_SYMBOLIC" #-} runSymbolicM ctx fid s
    modify $ \qs -> qs { qs_freeid = fid' }
    {-# SCC "MKFREES" #-} mapM mkfree frees
    {-# SCC "MKASSERTS" #-} mapM mkassert asserts
    return (Used (head ctx) v)
    
-- | Run the given query in its own scope and return the result.
nest :: SMT a -> SMT a
nest q = do
  freevars <- gets qs_freevars
  runCmds [SMT.Push]
  v <- q
  runCmds [SMT.Pop]
  modify $ \qs -> qs { qs_freevars = freevars }
  return v

-- | Update the free variables in the given expression based on the current
-- model.
realize :: ExpH -> Realize ExpH
realize e = {-# SCC "REALIZE" #-} Realize $ do
    freevars <- gets qs_freevars
    freevals <- gets qs_freevals
    fvs <- case freevals of
              Just vs -> return vs
              Nothing -> do
                freevals <- mapM realizefree freevars
                modify $ \qs -> qs { qs_freevals = Just freevals }
                return freevals
    let freemap = zip [n | Sig n _ <- freevars] fvs
        g e | VarEH (Sig nm _) <- force e = lookup nm freemap
            | otherwise = Nothing
    return $ if null freemap then e else transform g e

