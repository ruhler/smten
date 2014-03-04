
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of the Yices2 backend for smten.
module Smten.Compiled.Smten.Symbolic.Solver.Yices2 (yices2) where

import Foreign

import qualified Data.HashTable.IO as H

import Smten.Runtime.Yices2.FFI
import Smten.Runtime.Yices2.AST
import Smten.Runtime.Build
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.Solver

{-# SPECIALIZE build :: Yices2 -> BoolFF -> IO (YTerm, [(FreeID, Type)]) #-}
{-# SPECIALIZE solverFromAST :: IO Yices2 -> Solver #-}

yices2 :: Solver
yices2 = solverFromAST $ do
  r <- c_yices_load
  case r of
    0 -> return ()
    _ -> error "yices2 smten backend: unable to load libyices.so.2.1"
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  vars <- H.new
  return $ Yices2 ptr vars

