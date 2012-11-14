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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.SMT.Query (
    Answer(..), Realize(), 
    RunOptions(..), runQuery,
    Query, query, free, assert, queryS, realize, envQ, envR,
    ) where

import Data.Functor
import Data.List(nub)

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
import Seri.Elaborate hiding (query)
import qualified Seri.Elaborate


data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

derive_SeriT ''Answer
derive_SeriEH ''Answer

newtype Realize s a = Realize {
    runRealize :: Query s a
} deriving (Functor, Monad)

data QS s = QS {
    qs_ctx :: s,
    qs_dh :: Maybe Handle,
    qs_freeid :: Integer,
    qs_qs :: Compilation,
    qs_freevars :: [Sig],
    qs_freevals :: Maybe [ExpH], -- ^ Cache of free variable values
    qs_env :: Env
}

type Query s = StateT (QS s) IO

sendCmds :: (SMT.Solver s) => [SMT.Command] -> s -> Maybe Handle -> IO ()
sendCmds cmds ctx Nothing = mapM_ (SMT.run ctx) cmds
sendCmds cmds ctx (Just dh) = do
    hPutStr dh (unlines (map (SMT.pretty ctx) cmds))
    mapM_ (SMT.run ctx) cmds

runCmds :: (SMT.Solver s) => [SMT.Command] -> Query s ()
runCmds cmds = do
    ctx <- gets qs_ctx
    dh <- gets qs_dh
    lift $ sendCmds cmds ctx dh

check :: (SMT.Solver s) => Query s SMT.Result
check = do
    ctx <- gets qs_ctx
    debug (SMT.pretty ctx SMT.Check)
    res <- lift $ SMT.check ctx
    debug $ "; check returned: " ++ show res
    modify $ \qs -> qs { qs_freevals = Nothing }
    return res

-- Output a line to the debug output.
debug :: (SMT.Solver s) => String -> Query s ()
debug msg = do
    dh <- gets qs_dh
    case dh of
        Nothing -> return ()
        Just h -> lift $ hPutStrLn h msg

freevar :: (SMT.Solver s) => Query s Name
freevar = do
    fid <- gets qs_freeid
    modify $ \qs -> qs { qs_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> Name
freename id = name $ "free~" ++ show id

smtt :: (SMT.Solver s) => Type -> Query s SMT.Type
smtt t = do
    qs <- gets qs_qs 
    let mkyt = do
          yt <- smtT t
          cmds <- smtD
          return (cmds, yt)
    ((cmds, yt), qs') <- lift . attemptIO $ runCompilation mkyt qs
    modify $ \s -> s { qs_qs = qs' }
    runCmds cmds
    return yt

smte :: (SMT.Solver s) => ExpH -> Query s SMT.Expression
smte e = do
    qs <- gets qs_qs 
    let mkye = do
          ye <- smtE e
          cmds <- smtD
          return (cmds, ye)
    ((cmds, ye), qs') <- lift . attemptIO $ runCompilation mkye qs
    modify $ \s -> s { qs_qs = qs' }
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
    debugout :: Maybe FilePath
} deriving(Show)
            
mkQS :: (SMT.Solver s) => s -> RunOptions -> Env -> IO (QS s)
mkQS ctx opts env = do
    dh <- case debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)

    return $ QS {
        qs_ctx = ctx,
        qs_dh = dh,
        qs_freeid = 1,
        qs_qs = compilation env,
        qs_freevars = [],
        qs_freevals = Nothing,
        qs_env = env
    }

-- | Evaluate a query using the given environment.
-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
runQuery :: (SMT.Solver s) => RunOptions -> Env -> Query s a -> IO a
runQuery opts env q = do
    ctx <- SMT.initialize
    qs <- mkQS ctx opts env
    evalStateT q qs


-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the smt model.
--
-- Assumes:
--   Integers, Bools, and Bit vectors are implemented directly using the
--   corresponding smt primitives. (Should I not be assuming this?)
realizefree :: (SMT.Solver s) => Env -> Sig -> Query s ExpH
realizefree _ (Sig nm t) | t == boolT = do
    ctx <- gets qs_ctx
    bval <- lift $ SMT.getBoolValue ctx (smtN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show bval
    return (boolEH bval)
realizefree _ (Sig nm t) | t == integerT = do
    ctx <- gets qs_ctx
    ival <- lift $ SMT.getIntegerValue ctx (smtN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show ival
    return (integerEH ival)
realizefree _ (Sig nm (AppT (ConT n) (NumT (ConNT w)))) | n == name "Bit" = do
    ctx <- gets qs_ctx
    bval <- lift $ SMT.getBitVectorValue ctx w (smtN nm)
    debug $ "; " ++ pretty nm ++ " has value " ++ show bval
    return (bitEH (bv_make w bval))
realizefree _ (Sig _ t@(AppT (AppT (ConT n) _) _)) | n == name "->"
  = return (error $ "TODO: realizefree type " ++ pretty t)
realizefree _ (Sig _ t)
  = return (error $ "unexpected realizefree type: " ++ pretty t)

-- | Check if the current assertions are satisfiable. If so, runs the given
-- realize computation and returns that as the body of the Answer.
query :: (SMT.Solver s) => Realize s a -> Query s (Answer a)
query r = do
  res <- check
  case res of 
      SMT.Satisfiable -> Satisfiable <$> runRealize r
      SMT.Unsatisfiable -> return Unsatisfiable
      _ -> return Unknown

-- | Allocate a free expression of the given type.
free :: (SMT.Solver s) => Type -> Query s ExpH
free t | isPrimT t = do
  t' <- smtt t
  free <- freevar
  let freevar = Sig free t
  modify $ \qs -> qs { qs_freevars = freevar : qs_freevars qs }
  runCmds [SMT.Declare (smtN free) t']
  return (varEH freevar)
free t = do
  let (ConT dt, args) = de_appsT t
  env <- gets qs_env
  DataD _ vars cs <- lift . attemptIO $ lookupDataD env dt
  (let mkcon :: (SMT.Solver s) => Con -> Query s ExpH
       mkcon (Con cn ts) = 
         let ts' = assign (zip (map tyVarName vars) args) ts
         in do
             argvals <- mapM free ts'
             return $ appsEH (conEH (Sig cn (arrowsT (ts' ++ [t])))) argvals
  
       mkcons :: (SMT.Solver s) => [Con] -> Query s ExpH
       mkcons [] = error $ "free on DataD with no constructors: " ++ pretty t
       mkcons [c] = mkcon c
       mkcons (c:cs) = do
         isthis <- free boolT
         this <- mkcon c
         rest <- mkcons cs
         return $ ifEH isthis this rest
   in do
       v <- mkcons cs
       debug $ "; free " ++ pretty t ++ ":"
       debug . unlines . (map (';':)) . lines . pretty $ v
       return v
   )

-- | Assert the given seri boolean expression.
assert :: (SMT.Solver s) => ExpH -> Query s ()
assert p = do
  yp <- smte p
  runCmds [SMT.Assert yp]

-- | Run the given query in its own scope and return the result.
-- Note: it's possible to leak free variables with this function.
-- You should not return anything from the first query which could contain a
-- free variable, otherwise who knows what will happen.
queryS :: (SMT.Solver s) => Query s a -> Query s a
queryS q = do
  runCmds [SMT.Push]
  v <- q
  runCmds [SMT.Pop]
  return v

-- | Update the free variables in the given expression based on the current
-- model.
realize :: (SMT.Solver s) => ExpH -> Realize s ExpH
realize e = Realize $ do
    freevars <- gets qs_freevars
    freevals <- gets qs_freevals
    fvs <- case freevals of
              Just vs -> return vs
              Nothing -> do
                env <- gets qs_env
                freevals <- mapM (realizefree env) freevars
                modify $ \qs -> qs { qs_freevals = Just freevals }
                return freevals
    let freemap = zip freevars fvs
        g :: ExpH -> Maybe ExpH
        g e = do
            s <- de_varEH e
            lookup s freemap
    return $ Seri.Elaborate.transform g e

-- | Return the environment the query is running under.
envQ :: (SMT.Solver s) => Query s Env
envQ = gets qs_env

envR :: (SMT.Solver s) => Realize s Env
envR = Realize envQ

