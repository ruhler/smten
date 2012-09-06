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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Seri.SMT.Yices (
    RunOptions(..), SMTQuerier(), mkQuerier, runQuery,
    ) where

import Debug.Trace

import qualified Data.Map as Map
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices.Syntax as Y
import qualified Yices.Concrete as Y
import qualified Yices.Yices as Y

import Seri.Failable
import Seri.Lambda
import Seri.Target.Elaborate
import Seri.Target.Yices.Yices


data SMTQuerier y = SMTQuerier {
    ys_ctx :: y,
    ys_dh :: Maybe Handle,
    ys_freeid :: Integer,
    ys_ys :: Compilation,
    ys_env :: Env,

    -- yices can't redefine names when run directly, so even though it's not a
    -- problem with the C api, we make sure we don't ever try to redefine the
    -- same top level name. This map stores, for each previously defined top
    -- level name, an integer that can be appended to it to get a new version
    -- of the name that hasn't been defined yet.
    ys_topnms :: Map.Map Name Integer
}

type YicesMonad y = StateT (SMTQuerier y) IO

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

freevar :: YicesMonad y Name
freevar = do
    fid <- gets ys_freeid
    modify $ \ys -> ys { ys_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> Name
freename id = name $ "free~" ++ show id

isfreename :: Name -> Bool
isfreename nm = name "free~" == ntake 5 nm

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

topname :: Y.Yices y => Name -> YicesMonad y Name
topname n = do
    m <- gets ys_topnms
    let (id, m') = Map.insertLookupWithKey (\_ -> (+)) n 1 m
    modify $ \ys -> ys { ys_topnms = m' }
    return $ n `nappend` name "~t" `nappend` name (show (fromMaybe 0 id))
    
runQueryM :: Y.Yices y => Exp -> YicesMonad y Exp
runQueryM e = do
    env <- gets ys_env
    case elaborate WHNF env e of
        e@(AppE (LamE s@(Sig n t) b) x) -> do
            n' <- topname n
            t' <- yicest t
            x' <- yicese x
            runCmds [Y.Define (yicesN n') t' (Just x')]
            runQueryM (rename n n' b)
        (AppE (VarE (Sig n _)) arg) | n == name "Seri.SMT.SMT.query" -> do
            res <- check
            case res of 
                Y.Satisfiable -> do
                    arg' <- realize env arg
                    return $ AppE (ConE (Sig (name "Satisfiable") (AppT (ConT (name "Answer")) (typeof arg)))) arg'
                Y.Unsatisfiable-> return $ ConE (Sig (name "Unsatisfiable") (AppT (ConT (name "Answer")) (typeof arg)))
                _ -> return $ ConE (Sig (name "Unknown") (AppT (ConT (name "Answer")) (typeof arg)))
        (VarE (Sig n (AppT _ t))) | n == name "Seri.SMT.SMT.free" -> makefree t
        (AppE (VarE (Sig n _)) p) | n == name "Seri.SMT.SMT.assert" -> do
            yp <- yicese p
            true <- yicese trueE
            runCmds [Y.Assert (Y.eqE true yp)]
            return (ConE (Sig (name "()") (ConT (name "()"))))
        (AppE (VarE (Sig n _)) q) | n == name "Seri.SMT.SMT.queryS" -> do
            runCmds [Y.Push]
            x <- runQueryM q
            let q' = AppE (VarE (Sig (name "Seri.SMT.SMT.query") UnknownT)) x
            y <- runQueryM q'
            runCmds [Y.Pop]
            return y
        (AppE (VarE (Sig n _)) x) | n == name "Seri.SMT.SMT.return_query" -> return x
        (AppE (AppE (VarE (Sig n _)) x) f) | n == name "Seri.SMT.SMT.bind_query" -> do
          result <- runQueryM x
          runQueryM (AppE f result)
        (AppE (AppE (VarE (Sig n _)) x) y) | n == name "Seri.SMT.SMT.nobind_query" -> do
          runQueryM x
          runQueryM y
        x -> error $ "unknown Query: " ++ pretty x

data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    nocaseerr :: Bool
} deriving(Show)
            
-- | Construct a SMTQuerier object for running queries with yices under the given
-- seri environment.
mkQuerier :: (Y.Yices y) => RunOptions -> Env -> y -> IO (SMTQuerier y)
mkQuerier opts env ctx = do
    dh <- case debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)

    return $ SMTQuerier {
        ys_ctx = ctx,
        ys_dh = dh,
        ys_freeid = 1,
        ys_ys = compilation (Y.version ctx) (nocaseerr opts) env,
        ys_env = env,
        ys_topnms = Map.empty
    }

-- | Evaluate a query using the given environment.
-- Returns the result of the query and the updated querier.
runQuery :: (Y.Yices y) => SMTQuerier y -> Exp -> IO (Exp, SMTQuerier y)
runQuery q e = do
    (e', q') <- runStateT (runQueryM e) q
    return (elaborate WHNF (ys_env q') e', q')

isPrimT :: Type -> Bool
isPrimT t | t == boolT = True
isPrimT t | t == integerT = True
isPrimT (AppT (ConT n) _) | n == name "Bit" = True
isPrimT t | head (unappsT t) == ConT (name "->") = True
isPrimT _ = False

-- | Make a free value of the given type.
makefree :: Y.Yices y => Type -> YicesMonad y Exp
makefree t | isPrimT t = do
  t' <- yicest t
  free <- freevar
  runCmds [Y.Define (yicesN free) t' Nothing]
  return (VarE (Sig free t))
makefree t = do
  let (ConT dt):args = unappsT t
  env <- gets ys_env
  DataD _ vars cs <- lift . attemptIO $ lookupDataD env dt
  (let mkcon :: Y.Yices y => Con -> YicesMonad y Exp
       mkcon (Con cn ts) = 
         let ts' = assign (zip (map tyVarName vars) args) ts
         in do
             argvals <- mapM makefree ts'
             return $ appsE ((ConE (Sig cn (arrowsT (ts' ++ [t])))):argvals)
  
       mkcons :: Y.Yices y => [Con] -> YicesMonad y Exp
       mkcons [] = error $ "makefree on DataD with no constructors: " ++ pretty t
       mkcons [c] = mkcon c
       mkcons (c:cs) = do
         isthis <- makefree boolT
         this <- mkcon c
         rest <- mkcons cs
         return $ ifE isthis this rest
   in do
       v <- mkcons cs
       debug $ "; makefree " ++ pretty t ++ ":"
       debug . unlines . (map (';':)) . lines . pretty $ v
       return v
   )

-- | Given a free variable name and corresponding seri type, return the value
-- of that free variable from the yices model.
--
-- Assumes:
--   Integers, Bools, and Bit vectors are implemented directly using the
--   corresponding yices primitives. (Should I not be assuming this?)
realizefree :: Y.Yices y => Env -> Name -> Type -> YicesMonad y Exp
realizefree _ nm t | t == boolT = do
    ctx <- gets ys_ctx
    bval <- lift $ Y.getBoolValue ctx (yicesN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show bval
    return (boolE bval)
realizefree _ nm t | t == integerT = do
    ctx <- gets ys_ctx
    ival <- lift $ Y.getIntegerValue ctx (yicesN nm)
    debug $ "; " ++ pretty nm ++ " is " ++ show ival
    return (integerE ival)
realizefree _ nm (AppT (ConT n) (NumT (ConNT w))) | n == name "Bit" = do
    ctx <- gets ys_ctx
    bval <- lift $ Y.getBitVectorValue ctx w (yicesN nm)
    debug $ "; " ++ pretty nm ++ " has value " ++ show bval
    return (bitE w bval)
realizefree _ _ t@(AppT (AppT (ConT n) _) _) | n == name "->"
  = error $ "TODO: realizefree type " ++ pretty t
realizefree _ _ t
  = error $ "unexpected realizefree type: " ++ pretty t

data Realize = Realize Env

instance (Y.Yices y) => TransformerM Realize (YicesMonad y) where
    tm_Exp (Realize env) (VarE (Sig nm ty)) | isfreename nm = realizefree env nm ty
    tm_Exp _ e = return e

-- | Update the free variables in the given expression based on the current
-- yices model.
realize :: Y.Yices y => Env -> Exp -> YicesMonad y Exp
realize env = transformM (Realize env)


-- Rename free variables in the given expression with the given key name to
-- the given value name.
rename :: Name -> Name -> Exp -> Exp
rename k v | k == v = id
rename k v = 
  let re e@(LitE {}) = e
      re (CaseE x ms) = CaseE (re x) (map rm ms)
      re (AppE a b) = AppE (re a) (re b)
      re e@(LamE (Sig n _) b) | k == n = e
      re (LamE s b) = LamE s (re b)
      re e@(ConE {}) = e
      re (VarE (Sig n t)) | n == k = VarE (Sig v t)
      re e@(VarE {}) = e

      rm m@(Match p _) | k `elem` bindingsP' p = m
      rm (Match p b) = Match p (re b)
  in re



