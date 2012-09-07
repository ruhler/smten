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

module Seri.SMT.Query (
    Answer(..), Query, Realize, 
    query, queryS, free, assert, realize,
    RunOptions(..), runQuery,
    envQ,
    ) where

import Debug.Trace

import Data.Functor
import Data.Maybe

import System.IO

import Control.Monad.State
import qualified Yices.Syntax as Y
import qualified Yices.Concrete as Y
import qualified Yices.Yices as Y
import qualified Yices.Yices2 as Y

import Seri.Failable
import Seri.Lambda hiding (free, query)
import Seri.Target.Yices.Yices


data YS = YS {
    ys_ctx :: Y.Yices2FFI,
    ys_dh :: Maybe Handle,
    ys_freeid :: Integer,
    ys_ys :: Compilation,
    ys_env :: Env
}

type Query = StateT YS IO
type Realize = Query

data Answer a = Satisfiable a | Unsatisfiable | Unknown
    deriving (Eq, Show)

sendCmds :: [Y.Command] -> Y.Yices2FFI -> Maybe Handle -> IO ()
sendCmds cmds ctx Nothing = mapM_ (Y.run ctx) cmds
sendCmds cmds ctx (Just dh) = do
    hPutStr dh (unlines (map (Y.pretty (Y.version ctx)) cmds))
    mapM_ (Y.run ctx) cmds

runCmds :: [Y.Command] -> Query ()
runCmds cmds = do
    ctx <- gets ys_ctx
    dh <- gets ys_dh
    lift $ sendCmds cmds ctx dh

check :: Query Y.Result
check = do
    ctx <- gets ys_ctx
    debug (Y.pretty (Y.version ctx) Y.Check)
    res <- lift $ Y.check ctx
    debug $ "; check returned: " ++ show res
    return res

-- Output a line to the debug output.
debug :: String -> Query ()
debug msg = do
    dh <- gets ys_dh
    case dh of
        Nothing -> return ()
        Just h -> lift $ hPutStrLn h msg

freevar :: Query Name
freevar = do
    fid <- gets ys_freeid
    modify $ \ys -> ys { ys_freeid = fid+1 }
    return $ freename fid

freename :: Integer -> Name
freename id = name $ "free~" ++ show id

isfreename :: Name -> Bool
isfreename nm = name "free~" == ntake 5 nm

yicest :: Type -> Query Y.Type
yicest t = do
    ys <- gets ys_ys 
    ((cmds, yt), ys') <- lift . attemptIO $ runCompilation (yicesT t) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return yt

yicese :: Exp -> Query Y.Expression
yicese e = do
    ys <- gets ys_ys 
    ((cmds, ye), ys') <- lift . attemptIO $ runCompilation (yicesE e) ys
    modify $ \s -> s { ys_ys = ys' }
    runCmds cmds
    return ye

query :: Realize a -> Query (Answer a)
query r = do
  res <- check
  case res of 
      Y.Satisfiable -> Satisfiable <$> r
      Y.Unsatisfiable -> return Unsatisfiable
      _ -> return Unknown

free :: Type -> Query Exp
free = makefree

assert :: Exp -> Query ()
assert p = do
  yp <- yicese p
  runCmds [Y.Assert yp]

queryS :: Query a -> Query a
queryS q = do
  runCmds [Y.Push]
  v <- q
  runCmds [Y.Pop]
  return v

data RunOptions = RunOptions {
    debugout :: Maybe FilePath,
    nocaseerr :: Bool
} deriving(Show)
            
mkYS :: RunOptions -> Env -> IO YS
mkYS opts env = do
    ctx <- Y.mkYices
    dh <- case debugout opts of
            Nothing -> return Nothing
            Just dbgfile -> do
                h <- openFile dbgfile WriteMode
                hSetBuffering h NoBuffering
                return (Just h)

    return $ YS {
        ys_ctx = ctx,
        ys_dh = dh,
        ys_freeid = 1,
        ys_ys = compilation (Y.version ctx) (nocaseerr opts) env,
        ys_env = env
    }

-- | Evaluate a query using the given environment.
runQuery :: RunOptions -> Env -> Query a -> IO a
runQuery opts env q = do
    ys <- mkYS opts env
    evalStateT q ys

isPrimT :: Type -> Bool
isPrimT t | t == boolT = True
isPrimT t | t == integerT = True
isPrimT (AppT (ConT n) _) | n == name "Bit" = True
isPrimT t | head (unappsT t) == ConT (name "->") = True
isPrimT _ = False

-- | Make a free value of the given type.
makefree :: Type -> Query Exp
makefree t | isPrimT t = do
  t' <- yicest t
  free <- freevar
  runCmds [Y.Define (yicesN free) t' Nothing]
  return (VarE (Sig free t))
makefree t = do
  let (ConT dt):args = unappsT t
  env <- gets ys_env
  DataD _ vars cs <- lift . attemptIO $ lookupDataD env dt
  (let mkcon :: Con -> Query Exp
       mkcon (Con cn ts) = 
         let ts' = assign (zip (map tyVarName vars) args) ts
         in do
             argvals <- mapM makefree ts'
             return $ appsE ((ConE (Sig cn (arrowsT (ts' ++ [t])))):argvals)
  
       mkcons :: [Con] -> Query Exp
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
realizefree :: Env -> Name -> Type -> Query Exp
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

data RealizeT = RealizeT Env

instance TransformerM RealizeT Query where
    tm_Exp (RealizeT env) (VarE (Sig nm ty)) | isfreename nm = realizefree env nm ty
    tm_Exp _ e = return e

-- | Update the free variables in the given expression based on the current
-- yices model.
-- This requires (and assumes) check was just called and return satisfiable.
realize :: Exp -> Realize Exp
realize e = do
    env <- gets ys_env
    transformM (RealizeT env) e

envQ :: Query Env
envQ = gets ys_env

