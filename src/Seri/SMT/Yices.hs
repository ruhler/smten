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

module Seri.SMT.Yices (
    RunOptions(..), Querier(), mkQuerier, runQuery,
    ) where

import Data.Generics
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices.Syntax as Y
import qualified Yices.Yices as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Elaborate
import Seri.Target.Yices.Yices


data Querier y = Querier {
    ys_ctx :: y,
    ys_dh :: Maybe Handle,
    ys_freeid :: Integer,
    ys_ys :: Compilation,
    ys_env :: Env
}

type YicesMonad y = StateT (Querier y) IO

sendCmds :: Y.Yices y => [Y.Command] -> y -> Maybe Handle -> IO ()
sendCmds cmds ctx Nothing = mapM_ (Y.run ctx) cmds
sendCmds cmds ctx (Just dh) = do
    hPutStr dh (unlines (map (Y.pretty (Y.version ctx)) cmds))
    mapM_ (Y.run ctx) cmds

runCmds :: Y.Yices y => [Y.Command] -> YicesMonad y ()
runCmds cmds = do
    ctx <- gets ys_ctx
    dh <- gets ys_dh
    lift $ sendCmds cmds ctx dh

check :: (Y.Yices y) => YicesMonad y Y.Result
check = do
    ctx <- gets ys_ctx
    debug (Y.pretty (Y.version ctx) Y.Check)
    res <- lift $ Y.check ctx
    debug $ "; check returned: " ++ show res
    return res

-- Output a line to the debug output.
debug :: String -> YicesMonad y ()
debug msg = do
    dh <- gets ys_dh
    case dh of
        Nothing -> return ()
        Just h -> lift $ hPutStrLn h msg

freevar :: YicesMonad y String
freevar = do
    fid <- gets ys_freeid
    modify $ \ys -> ys { ys_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> String
freename id = "free~" ++ show id

isfreename :: String -> Bool
isfreename nm = "free~" == take 5 nm

yicest :: Y.Yices y => Type -> YicesMonad y Y.Type
yicest t = do
    ys <- gets ys_ys 
    ((cmds, yt), ys') <- lift . attemptIO $ runCompilation (yicesT t) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return yt

yicese :: Y.Yices y => Exp -> YicesMonad y Y.Expression
yicese e = do
    ys <- gets ys_ys 
    ((cmds, ye), ys') <- lift . attemptIO $ runCompilation (yicesE e) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return ye
    
runQueryM :: Y.Yices y => Exp -> YicesMonad y Exp
runQueryM e = do
    env <- gets ys_env
    case elaborate Simple env e of
        (AppE (VarE (Sig "Seri.SMT.SMT.query" _)) arg) -> do
            res <- check
            case res of 
                Y.Satisfiable -> do
                    arg' <- realize env arg
                    return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) arg'
                Y.Unsatisfiable-> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
                _ -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
        (VarE (Sig "Seri.SMT.SMT.free" (AppT (ConT "Query") t))) -> do
            t' <- yicest t
            free <- freevar
            runCmds [Y.Define (yicesN free) t' Nothing]
            return (VarE (Sig free t))
        (AppE (VarE (Sig "Seri.SMT.SMT.assert" _)) p) -> do
            yp <- yicese p
            true <- yicese trueE
            runCmds [Y.Assert (Y.eqE true yp)]
            return (ConE (Sig "()" (ConT "()")))
        (AppE (VarE (Sig "Seri.SMT.SMT.queryS" _)) q) -> do
            runCmds [Y.Push]
            x <- runQueryM q
            let q' = AppE (VarE (Sig "Seri.SMT.SMT.query" undefined)) x
            y <- runQueryM q'
            runCmds [Y.Pop]
            return y
        (AppE (VarE (Sig "Seri.SMT.SMT.return_query" _)) x) -> return x
        (AppE (AppE (VarE (Sig "Seri.SMT.SMT.bind_query" _)) x) f) -> do
          result <- runQueryM x
          runQueryM (AppE f result)
        (AppE (AppE (VarE (Sig "Seri.SMT.SMT.nobind_query" _)) x) y) -> do
          runQueryM x
          runQueryM y
        x -> error $ "unknown Query: " ++ pretty x

data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    inlinedepth :: Integer
} deriving(Show)
            
-- | Construct a Querier object for running queries with yices under the given
-- seri environment.
mkQuerier :: (Y.Yices y) => RunOptions -> Env -> y -> IO (Querier y)
mkQuerier opts env ctx = do
    dh <- case debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)

    return $ Querier {
        ys_ctx = ctx,
        ys_dh = dh,
        ys_freeid = 1,
        ys_ys = compilation (Y.version ctx) (inlinedepth opts) env,
        ys_env = env
    }

-- | Evaluate a query using the given environment.
-- Returns the result of the query and the updated querier.
runQuery :: (Y.Yices y) => Querier y -> Exp -> IO (Exp, Querier y)
runQuery q e = runStateT (runQueryM e) q


-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the yices model.
realizefree :: Y.Yices y => Env -> String -> Type -> YicesMonad y Exp
realizefree _ nm t | t == integerT = do
    debug $ "; realize integer: " ++ nm
    res <- check
    case res of
        Y.Satisfiable -> return ()
        _ -> error $ "realize free expected Satisfiable, but wasn't"
    ctx <- gets ys_ctx
    ival <- lift $ Y.getIntegerValue ctx (yicesN nm)
    debug $ "; " ++ nm ++ " is " ++ show ival
    return (integerE ival)
realizefree _ nm t@(AppT (AppT (ConT "->") _) _)
  = error $ "TODO: realizefree type " ++ pretty t
realizefree env nm t =
  let ConT dt = head $ unappsT t
      trycons :: Y.Yices y => [Con] -> YicesMonad y Exp
      trycons [] = return $ VarE (Sig "undefined" t)
      trycons (Con cn ts : cs) = do
            runCmds [Y.Push]
            free <- sequence $ replicate (length ts) freevar
            yts <- mapM yicest ts
            runCmds [Y.Define (yicesN f) yt Nothing | (f, yt) <- zip free yts]
            let args = [VarE (Sig n t) | (n, t) <- zip free ts]
            want <- yicese (appsE $ (ConE (Sig cn (ConT dt))) : args)
            runCmds [Y.Assert (Y.eqE (Y.varE (yicesN nm)) want)]
            res <- check
            case res of
                Y.Satisfiable -> do
                    argvals <- sequence [realizefree env n t | (n, t) <- zip free ts]
                    runCmds [Y.Pop]
                    return (appsE $ (ConE (Sig cn (ConT dt))) : argvals)
                _ -> do
                    runCmds [Y.Pop]
                    trycons cs
            
  in do
    debug $ "; realize: " ++ nm ++ " :: " ++ pretty t
    DataD _ _ cs <- lift . attemptIO $ lookupDataD env dt
    trycons cs
    

-- | Update the free variables in the given expression based on the current
-- yices model.
realize :: Y.Yices y => Env -> Exp -> YicesMonad y Exp
realize env = 
  let f :: Y.Yices y => Exp -> YicesMonad y Exp
      f (VarE (Sig nm ty)) | isfreename nm = realizefree env nm ty
      f e = return e
  in everywhereM (mkM f)

