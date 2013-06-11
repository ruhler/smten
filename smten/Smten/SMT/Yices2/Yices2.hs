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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices2 solver
module Smten.SMT.Yices2.Yices2 (yices2) where

import Foreign
import Foreign.C.String

import Data.Dynamic
import Data.Functor

import Smten.SMT.Yices2.FFI
import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S

box :: YTerm -> Dynamic
box = toDyn

unbox :: Dynamic -> YTerm
unbox = flip fromDyn (error "Yices2.unbox")

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
          S.assert = A.assert y2,
          S.declare_bool = y2declare_bool y2,
          S.declare_integer = y2declare_integer y2,
          S.declare_bit = y2declare_bit y2,
          S.getBoolValue = getBoolValue y2,
          S.getIntegerValue = getIntegerValue y2,
          S.getBitVectorValue = getBitVectorValue y2,
          S.check = check y2
       }

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

y2declare_bit :: Yices2 -> String -> Integer -> IO ()
y2declare_bit y nm w = do
    ty <- c_yices_bv_type (fromInteger w)
    term <- c_yices_new_uninterpreted_term ty
    withCString nm $ c_yices_set_term_name term

withy2 :: Yices2 -> (Ptr YContext -> IO a) -> IO a
withy2 y f = f (y2_ctx y)

check :: Yices2 -> IO S.Result
check y = withy2 y $ \ctx -> do
    st <- c_yices_check_context ctx nullPtr
    return $! fromYSMTStatus st

ite :: Yices2 -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic
ite _ p a b = box <$> c_yices_ite (unbox p) (unbox a) (unbox b)

instance AST Yices2 where
  assert y e = withy2 y $ \ctx -> c_yices_assert_formula ctx (unbox e)

  bool _ p = box <$> if p then c_yices_true else c_yices_false
  integer _ i = box <$> c_yices_int64 (fromInteger i)
  bit _ w v = 
        let w' = fromInteger w
            v' = fromInteger v
        in box <$> c_yices_bvconst_uint64 w' v'

  var _ nm = box <$> withCString nm c_yices_get_term_by_name

  ite_bool = ite
  ite_integer = ite
  ite_bit = ite

  eq_integer = bp c_yices_eq
  leq_integer = bp c_yices_arith_leq_atom
  add_integer = bp c_yices_add
  sub_integer = bp c_yices_sub

  eq_bit = bp c_yices_eq
  leq_bit = bp c_yices_bvle_atom
  add_bit = bp c_yices_bvadd
  sub_bit = bp c_yices_bvsub
  mul_bit = bp c_yices_bvmul
  or_bit = bp c_yices_bvor

bp :: (YTerm -> YTerm -> IO YTerm) -> Yices2 -> Dynamic -> Dynamic -> IO Dynamic
bp f _ a b = box <$> f (unbox a) (unbox b)

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

getBitVectorValue :: Yices2 -> String -> Integer -> IO Integer
getBitVectorValue y nm w = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        term <- withCString nm c_yices_get_term_by_name
        ir <- c_yices_get_bv_value model term ptr
        if ir == 0
            then peekArray (fromInteger w) ptr
            else error $ "yices2 get bit vector value returned: " ++ show ir
    c_yices_free_model model
    return $! bvInteger bits
        
bvInteger :: [Int32] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

