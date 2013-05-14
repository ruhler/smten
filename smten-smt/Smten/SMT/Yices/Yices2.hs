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
module Smten.SMT.Yices.Yices2 (yices2) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Data.IORef

import Smten.SMT.Yices.FFI2
import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S
import Smten.Name
import Smten.Sig
import Smten.Lit
import Smten.Type
import Smten.Bit
import Smten.ExpH
import qualified Smten.HashTable as HT

data Yices2 = Yices2 {
    y2_ctx :: Ptr YContext,
    y2_nid :: IORef Integer
}

-- TODO: this currently leaks context pointers!
-- That should most certainly be fixed somehow.
-- TODO: when do we call c_yices_exit?
yices2 :: IO S.Solver
yices2 = do
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  nid <- newIORef 0
  return $    
    let y2 = Yices2 ptr nid
    in S.Solver {
          S.push = push y2,
          S.pop = pop y2,
          S.fresh = y2fresh y2,
          S.assert = y2assert y2,
          S.check = check y2,
          S.getIntegerValue = getIntegerValue y2,
          S.getBoolValue = getBoolValue y2,
          S.getBitVectorValue = getBitVectorValue y2
       }

y2fresh :: Yices2 -> Type -> IO Name
y2fresh y ty = do
    nid <- readIORef (y2_nid y)
    modifyIORef' (y2_nid y) (+ 1)
    let nm = "f~" ++ show nid
    ty' <- ytype ty
    term <- c_yices_new_uninterpreted_term ty'
    withCString nm $ c_yices_set_term_name term
    return $ name nm

y2assert :: Yices2 -> ExpH -> IO ()
y2assert = A.assert

withy2 :: Yices2 -> (Ptr YContext -> IO a) -> IO a
withy2 y f = f (y2_ctx y)

push :: Yices2 -> IO ()
push y = withy2 y c_yices_push

pop :: Yices2 -> IO ()
pop y = withy2 y c_yices_pop

check :: Yices2 -> IO S.Result
check y = withy2 y $ \ctx -> do
    st <- c_yices_check_context ctx nullPtr
    return $! fromYSMTStatus st

getIntegerValue :: Yices2 -> Name -> IO Integer
getIntegerValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString (unname nm) c_yices_get_term_by_name
            ir <- c_yices_get_int64_value model term ptr
            if ir == 0
               then do 
                  v <- peek ptr
                  return $! v
               else error $ "yices2 get int64 value returned: " ++ show ir
    c_yices_free_model model
    return $! toInteger x

getBoolValue :: Yices2 -> Name -> IO Bool
getBoolValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    x <- alloca $ \ptr -> do
            term <- withCString (unname nm) c_yices_get_term_by_name
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
    
getBitVectorValue :: Yices2 -> Integer -> Name -> IO Integer
getBitVectorValue y w nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        term <- withCString (unname nm) c_yices_get_term_by_name
        ir <- c_yices_get_bv_value model term ptr
        if ir == 0
            then peekArray (fromInteger w) ptr
            else error $ "yices2 get bit vector value returned: " ++ show ir
    c_yices_free_model model
    return $! bvInteger bits
        
bvInteger :: [Int32] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

ytype :: Type -> IO YType
ytype t
 | Just i <- de_bitT t = c_yices_bv_type (fromIntegral i)
 | t == integerT = c_yices_int_type
 | t == boolT = c_yices_bool_type

instance AST Yices2 YTerm where
  fresh = y2fresh
  assert y e = withy2 y $ \ctx -> c_yices_assert_formula ctx e

  literal _ l
    | Just i <- de_integerL l = c_yices_int64 (fromInteger i)
    | Just bv <- de_bitL l =
        let w = fromInteger $ bv_width bv
            v = fromInteger $ bv_value bv
        in c_yices_bvconst_uint64 w v

  bool _ p = if p then c_yices_true else c_yices_false
  var _ nm = withCString (unname nm) c_yices_get_term_by_name
  ite _ = c_yices_ite 
  unary = HT.table [(name "Smten.Bit.__prim_not_Bit", const c_yices_bvnot)]
  binary = HT.table [
    (name "Prelude.__prim_eq_Integer", const c_yices_eq),
    (name "Prelude.__prim_add_Integer", const c_yices_add),
    (name "Prelude.__prim_sub_Integer", const c_yices_sub),
    (name "Prelude.__prim_mul_Integer", const c_yices_mul),
    (name "Prelude.__prim_lt_Integer", const c_yices_arith_lt_atom),
    (name "Prelude.__prim_leq_Integer", const c_yices_arith_leq_atom),
    (name "Prelude.__prim_gt_Integer", const c_yices_arith_gt_atom),
    (name "Prelude.__prim_geq_Integer", const c_yices_arith_geq_atom),
    (name "Smten.Bit.__prim_eq_Bit", const c_yices_eq),
    (name "Smten.Bit.__prim_lt_Bit", const c_yices_bvlt_atom),
    (name "Smten.Bit.__prim_leq_Bit", const c_yices_bvle_atom),
    (name "Smten.Bit.__prim_gt_Bit", const c_yices_bvgt_atom),
    (name "Smten.Bit.__prim_geq_Bit", const c_yices_bvge_atom),
    (name "Smten.Bit.__prim_add_Bit", const c_yices_bvadd),
    (name "Smten.Bit.__prim_sub_Bit", const c_yices_bvsub),
    (name "Smten.Bit.__prim_mul_Bit", const c_yices_bvmul),
    (name "Smten.Bit.__prim_or_Bit", const c_yices_bvor),
    (name "Smten.Bit.__prim_and_Bit", const c_yices_bvand),
    (name "Smten.Bit.__prim_concat_Bit", const c_yices_bvconcat),
    (name "Smten.Bit.__prim_shl_Bit", const c_yices_bvshl),
    (name "Smten.Bit.__prim_lshr_Bit", const c_yices_bvlshr)
    ]

  zeroextend _ a' n = c_yices_zero_extend a' (fromInteger n)
  signextend _ a' n = c_yices_sign_extend a' (fromInteger n)
  extract _ x' begin end = c_yices_bvextract x' (fromInteger begin) (fromInteger end)
  truncate _ x' w = c_yices_bvextract x' (fromInteger $ w-1) 0

