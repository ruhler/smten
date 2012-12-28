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

module Seri.SMT.Query (
    Answer(..), Realize(), 
    RunOptions(..), runQuery,
    Query, query, free, assert, queryS, realize,
    ) where

import Debug.Trace

import Data.Functor
import Data.List(nub)
import Data.Typeable

import System.IO

import Control.Monad.State

import qualified Seri.SMT.Syntax as SMT
import qualified Seri.SMT.Solver as SMT

import Seri.Bit
import Seri.Failable
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.ExpH
import Seri.Dec
import Seri.Ppr
import Seri.SMT.Translate


data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

derive_SeriT ''Answer
derive_SeriEH ''Answer

newtype Realize a = Realize {
    runRealize :: Query a
} deriving (Functor, Monad)

data QS = QS {
    qs_solver :: SMT.Solver,
    qs_dh :: Maybe Handle,
    qs_freeid :: Integer,
    qs_qs :: Compilation,
    qs_freevars :: [Sig],
    qs_freevals :: Maybe [ExpH] -- ^ Cache of free variable values
}

newtype Query a = Query (StateT QS IO a)
    deriving (Functor, Monad, MonadIO, Typeable)

deriving instance MonadState QS Query

sendCmds :: [SMT.Command] -> SMT.Solver -> Maybe Handle -> IO ()
sendCmds cmds solver Nothing = mapM_ (SMT.run solver) cmds
sendCmds cmds solver (Just dh) = do
    hPutStr dh (unlines (map (SMT.pretty solver) cmds))
    mapM_ (SMT.run solver) cmds

runCmds :: [SMT.Command] -> Query ()
runCmds cmds = {-# SCC "RunCmds" #-} do
    solver <- gets qs_solver
    dh <- gets qs_dh
    liftIO $ sendCmds cmds solver dh

check :: Query SMT.Result
check = {-# SCC "Check" #-} do
    solver <- gets qs_solver
    debug (SMT.pretty solver SMT.Check)
    res <- liftIO $ SMT.check solver
    debug $ "; check returned: " ++ show res
    modify $ \qs -> qs { qs_freevals = Nothing }
    return res

-- Output a line to the debug output.
debug :: String -> Query ()
debug msg = do
    dh <- gets qs_dh
    case dh of
        Nothing -> return ()
        Just h -> liftIO $ hPutStrLn h msg

freevar :: Query Name
freevar = do
    fid <- gets qs_freeid
    modify $ \qs -> qs { qs_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> Name
freename id = name $ "free~" ++ show id

smtt :: Type -> Query SMT.Type
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

smte' :: ExpH -> Query ([SMT.Command], SMT.Expression)
smte' e = {-# SCC "SmtE" #-} do
    qs <- gets qs_qs 
    let se = fromExpH e
        mkye :: CompilationM ([SMT.Command], SMT.Expression)
        mkye = do
          ye <- smtE se
          cmds <- smtD
          return (cmds, ye)
    ((cmds, ye), qs') <- liftIO . attemptIO $ runCompilation mkye qs
    modify $ \s -> s { qs_qs = qs' }
    return (cmds, ye)

smte :: ExpH -> Query SMT.Expression
smte e = do
    (cmds, ye) <- smte' e
    runCmds cmds
    return ye


isPrimT :: Type -> Bool
isPrimT t | t == boolT = True
isPrimT t | t == integerT = True
isPrimT (AppT (ConT n) _) | n == name "Bit" = True
isPrimT t | Just _ <- de_arrowT t = True
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

    return $ QS {
        qs_solver = ro_solver opts,
        qs_dh = dh,
        qs_freeid = 1,
        qs_qs = compilation,
        qs_freevars = [],
        qs_freevals = Nothing
    }

-- | Evaluate a query.
-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
runQuery :: RunOptions -> Query a -> IO a
runQuery opts (Query q) = do
    qs <- mkQS opts
    evalStateT q qs


-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the smt model.
--
-- Assumes:
--   Integers, Bools, and Bit vectors are implemented directly using the
--   corresponding smt primitives. (Should I not be assuming this?)
realizefree :: Sig -> Query ExpH
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
realizefree (Sig nm (AppT (ConT n) (NumT wt))) | n == name "Bit" = do
    let w = nteval wt
    solver <- gets qs_solver
    bval <- liftIO $ SMT.getBitVectorValue solver w (smtN nm)
    debug $ "; " ++ pretty nm ++ " has value " ++ show bval
    return (bitEH (bv_make w bval))
realizefree (Sig _ t@(AppT (AppT (ConT n) _) _)) | n == name "->"
  = return (error $ "TODO: realizefree type " ++ pretty t)
realizefree (Sig _ t)
  = return (error $ "unexpected realizefree type: " ++ pretty t)

-- | Check if the current assertions are satisfiable. If so, runs the given
-- realize computation and returns that as the body of the Answer.
query :: Realize a -> Query (Answer a)
query r = do
  res <- check
  case res of 
      SMT.Satisfiable -> Satisfiable <$> runRealize r
      SMT.Unsatisfiable -> return Unsatisfiable
      _ -> return Unknown

-- | Allocate a free expression of the given type.
free :: Type -> Query ExpH
free t | isPrimT t = do
  t' <- smtt t
  free <- freevar
  let freevar = Sig free t
  modify $ \qs -> qs { qs_freevars = freevar : qs_freevars qs }
  runCmds [SMT.Declare (smtN free) t']
  return (varEH freevar)
free t = error $ "Query.free: unsupported type: " ++ pretty t

-- | Assert the given seri boolean expression.
assert :: ExpH -> Query ()
assert p = do
  yp <- smte p
  runCmds [SMT.Assert yp]

-- | Run the given query in its own scope and return the result.
-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
queryS :: Query a -> Query a
queryS q = do
  runCmds [SMT.Push]
  v <- q
  runCmds [SMT.Pop]
  return v

-- | Update the free variables in the given expression based on the current
-- model.
realize :: ExpH -> Realize ExpH
realize e = Realize $ do
    freevars <- gets qs_freevars
    freevals <- gets qs_freevals
    fvs <- case freevals of
              Just vs -> return vs
              Nothing -> do
                freevals <- mapM realizefree freevars
                modify $ \qs -> qs { qs_freevals = Just freevals }
                return freevals
    let freemap = zip [n | Sig n _ <- freevars] fvs
        g (VarEH (Sig nm _)) = lookup nm freemap
        g _ = Nothing
    return $ if null freemap then e else transform g e

