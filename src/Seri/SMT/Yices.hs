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

module Seri.SMT.Yices (RunOptions(..), runYices) where

import Data.Generics
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices.Yices as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Elaborate
import Seri.Target.Yices.Yices


data YicesState = YicesState {
    ys_ctx :: Y.Context,
    ys_dh :: Handle,
    ys_freeid :: Integer,
    ys_ys :: Compilation
}

type YicesMonad = StateT YicesState IO

sendCmds :: [Y.CmdY] -> Y.Context -> Handle -> IO ()
sendCmds cmds ctx dh = do
    hPutStr dh (unlines (map show cmds))
    Y.runCmds ctx cmds

runCmds :: [Y.CmdY] -> YicesMonad ()
runCmds cmds = do
    ctx <- gets ys_ctx
    dh <- gets ys_dh
    lift $ sendCmds cmds ctx dh

check :: YicesMonad Y.Result
check = do
    debug "(check)"
    ctx <- gets ys_ctx
    res <- lift $ Y.check ctx
    debug $ "; check returned: " ++ show res
    return res

-- Output a line to the debug output.
debug :: String -> YicesMonad ()
debug msg = do
    dh <- gets ys_dh
    lift $ hPutStrLn dh msg

freevar = do
    fid <- gets ys_freeid
    modify $ \ys -> ys { ys_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> String
freename id = "free~" ++ show id

isfreename :: String -> Bool
isfreename nm = "free~" == take 5 nm

yicest :: Type -> YicesMonad Y.TypY
yicest t = do
    ys <- gets ys_ys 
    ((cmds, yt), ys') <- lift . attemptIO $ runCompilation (yicesT t) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return yt

yicese :: Exp -> YicesMonad Y.ExpY
yicese e = do
    ys <- gets ys_ys 
    ((cmds, ye), ys') <- lift . attemptIO $ runCompilation (yicesE e) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return ye
    
runQuery :: Rule YicesMonad -> Env -> Exp -> YicesMonad Exp
runQuery gr env e = do
    elaborated <- elaborate gr env e
    case elaborated of
        (AppE (VarE (Sig "Seri.SMT.SMT.query" _)) arg) -> do
            res <- check
            case res of 
                Y.Undefined -> return $ ConE (Sig "Unknown" (AppT (ConT "Answer") (typeof arg)))
                Y.Satisfiable -> do
                    arg' <- realize env arg
                    return $ AppE (ConE (Sig "Satisfiable" (AppT (ConT "Answer") (typeof arg)))) arg'
                _ -> return $ ConE (Sig "Unsatisfiable" (AppT (ConT "Answer") (typeof arg)))
        (VarE (Sig "Seri.SMT.SMT.free" (AppT (ConT "Query") t))) -> do
            t' <- yicest t
            free <- freevar
            runCmds [Y.DEFINE (yicesN free, t') Nothing]
            return (VarE (Sig free t))
        (AppE (VarE (Sig "Seri.SMT.SMT.assert" _)) p) -> do
            yp <- yicese p
            true <- yicese trueE
            runCmds [Y.ASSERT (true Y.:= yp)]
            return (ConE (Sig "()" (ConT "()")))
        (AppE (VarE (Sig "Seri.SMT.SMT.queryS" _)) q) -> do
            runCmds [Y.PUSH]
            x <- runQuery gr env q
            let q' = AppE (VarE (Sig "Seri.SMT.SMT.query" undefined)) x
            y <- runQuery gr env q'
            runCmds [Y.POP]
            return y
        (AppE (VarE (Sig "Seri.SMT.SMT.return_query" _)) x) -> return x
        (AppE (AppE (VarE (Sig "Seri.SMT.SMT.bind_query" _)) x) f) -> do
          result <- runQuery gr env x
          runQuery gr env (AppE f result)
        (AppE (AppE (VarE (Sig "Seri.SMT.SMT.nobind_query" _)) x) y) -> do
          runQuery gr env x
          runQuery gr env y
        x -> error $ "unknown Query: " ++ pretty x


data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    inlinedepth :: Integer
} deriving(Show)
            
runYices :: Rule YicesMonad -> RunOptions -> Env -> Exp -> IO Exp
runYices gr opts env e = do
    dh <- openFile (fromMaybe "/dev/null" (debugout opts)) WriteMode

    -- We set NoBuffering, because otherwise the output gets chopped off for
    -- longer outputs.
    hSetBuffering dh NoBuffering
    ctx <- Y.mkContext

    let query = runQuery gr env e
    (x, _) <- runStateT query (YicesState {
        ys_ctx = ctx,
        ys_dh = dh,
        ys_freeid = 1,
        ys_ys = compilation (inlinedepth opts) env
    })
    hClose dh
    return x
    

-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the yices model.
realizefree :: Env -> String -> Type -> YicesMonad Exp
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
      trycons :: [Con] -> YicesMonad Exp
      trycons [] = return $ VarE (Sig "undefined" t)
      trycons (Con cn ts : cs) = do
            runCmds [Y.PUSH]
            free <- sequence $ replicate (length ts) freevar
            yts <- mapM yicest ts
            runCmds [Y.DEFINE (yicesN f, yt) Nothing | (f, yt) <- zip free yts]
            let args = [VarE (Sig n t) | (n, t) <- zip free ts]
            want <- yicese (appsE $ (ConE (Sig cn (ConT dt))) : args)
            runCmds [Y.ASSERT (Y.VarE (yicesN nm) Y.:= want)]
            res <- check
            case res of
                Y.Satisfiable -> do
                    argvals <- sequence [realizefree env n t | (n, t) <- zip free ts]
                    runCmds [Y.POP]
                    return (appsE $ (ConE (Sig cn (ConT dt))) : argvals)
                _ -> do
                    runCmds [Y.POP]
                    trycons cs
            
  in do
    debug $ "; realize: " ++ nm ++ " :: " ++ pretty t
    DataD _ _ cs <- lift . attemptIO $ lookupDataD env dt
    trycons cs
    

-- | Update the free variables in the given expression based on the current
-- yices model.
realize :: Env -> Exp -> YicesMonad Exp
realize env = 
  let f :: Exp -> YicesMonad Exp
      f (VarE (Sig nm ty)) | isfreename nm = realizefree env nm ty
      f e = return e
  in everywhereM (mkM f)

