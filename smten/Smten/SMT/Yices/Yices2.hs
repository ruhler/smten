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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices2 solver
module Smten.SMT.Yices.Yices2 (Yices2, yices2) where

import Foreign
import Foreign.C.String

import Smten.SMT.Yices.FFI2
import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S
import qualified Smten.Runtime.Prelude as R

data Yices2 = Yices2 {
    y2_ctx :: Ptr YContext
}

-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
-- TODO: when do we call c_yices_exit?
yices2 :: IO S.Solver
yices2 = do
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  return $    
    let y2 = Yices2 ptr
    in S.Solver {
          S.assert = y2assert y2,
          S.declare_bool = y2declare_bool y2,
          S.declare_integer = y2declare_integer y2,
          S.getBoolValue = getBoolValue y2,
          S.getIntegerValue = getIntegerValue y2,
          S.check = check y2
       }

y2assert :: Yices2 -> R.Bool -> IO ()
y2assert = A.assert

y2declare_bool :: Yices2 -> String -> IO ()
y2declare_bool y nm = do
    ty <- c_yices_bool_type
    term <- c_yices_new_uninterpreted_term ty
    withCString nm $ c_yices_set_term_name term

y2declare_integer :: Yices2 -> String -> IO ()
y2declare_integer y nm = do
    ty <- c_yices_int_type
    term <- c_yices_new_uninterpreted_term ty
    withCString nm $ c_yices_set_term_name term

withy2 :: Yices2 -> (Ptr YContext -> IO a) -> IO a
withy2 y f = f (y2_ctx y)

check :: Yices2 -> IO S.Result
check y = withy2 y $ \ctx -> do
    st <- c_yices_check_context ctx nullPtr
    return $! fromYSMTStatus st

instance AST Yices2 YTerm where
  assert y e = withy2 y $ \ctx -> c_yices_assert_formula ctx e
  bool _ p = if p then c_yices_true else c_yices_false
  integer _ i = c_yices_int64 (fromInteger i)
  var _ nm = withCString nm c_yices_get_term_by_name
  ite _ = c_yices_ite 
  eq_integer _ = c_yices_eq
  add_integer _ = c_yices_add

getBoolValue :: Yices2 -> String -> IO Bool
getBoolValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString nm c_yices_get_term_by_name
            ir <- c_yices_get_bool_value model term ptr
            case ir of
               _ | ir == (-1) -> do
                  -- -1 means we don't care, so just return the equivalent
                  -- of False.
                  return 0

               0 -> do 
                  v <- peek ptr
                  return v

               _ -> error $ "yices2 get bool value returned: " ++ show ir
    c_yices_free_model model
    case x of
        0 -> return False
        1 -> return True
        _ -> error $ "yices2 get bool value got: " ++ show x

getIntegerValue :: Yices2 -> String -> IO Integer
getIntegerValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString nm c_yices_get_term_by_name
            ir <- c_yices_get_int64_value model term ptr
            if ir == 0
               then do 
                  v <- peek ptr
                  return $! v
               else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return $! toInteger x

