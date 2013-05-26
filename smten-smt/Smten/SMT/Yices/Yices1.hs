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

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Backend for the Yices1 Solver
--
-- NOTE: This assumes all the symbols from the yices1 library starting with
-- yices_ have been renamed to yices1_. This is so yices1 and yices2 can
-- coexist.
module Smten.SMT.Yices.Yices1 (yices1) where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Foreign.Concurrent as F

import Data.IORef

import Smten.SMT.AST
import qualified Smten.SMT.Assert as A
import qualified Smten.SMT.Solver as S
import Smten.Name
import Smten.Lit
import Smten.Bit
import Smten.Type
import Smten.ExpH
import qualified Smten.HashTable as HT

data YContext

data YModel_
type YModel = Ptr YModel_

data YDecl_
type YDecl = Ptr YDecl_

data YExpr_
type YExpr = Ptr YExpr_

data Yices1 = Yices1 {
    y1_fp :: ForeignPtr YContext,
    y1_nid :: IORef Integer
}

type YBool = CInt

yFalse, yTrue, yUndef :: YBool
yFalse = -1
yTrue  = 1
yUndef = 0


foreign import ccall "yices1_mk_context"
    c_yices_mk_context  :: IO (Ptr YContext)

foreign import ccall "yices1_del_context"
    c_yices_del_context :: Ptr YContext -> IO ()

foreign import ccall "yices1_parse_command"
    c_yices_parse_command :: Ptr YContext -> CString -> IO Bool

foreign import ccall "yices1_parse_expression"
    c_yices_parse_expression :: Ptr YContext -> CString -> IO YExpr

foreign import ccall "yices1_check"
    c_yices_check :: Ptr YContext -> IO YBool

foreign import ccall "yices1_push"
    c_yices_push :: Ptr YContext -> IO ()

foreign import ccall "yices1_pop"
    c_yices_pop :: Ptr YContext -> IO ()

foreign import ccall "yices1_assert"
    c_yices_assert :: Ptr YContext -> YExpr -> IO ()

foreign import ccall "yices1_get_model"
    c_yices_get_model :: Ptr YContext -> IO YModel

foreign import ccall "yices1_display_model"
    c_yices_display_model :: YModel -> IO ()

foreign import ccall "yices1_enable_type_checker"
    c_yices_enable_type_checker :: Bool -> IO ()

foreign import ccall "yices1_get_last_error_message"
    c_yices_get_last_error_message :: IO CString

foreign import ccall "yices1_get_value"
    c_yices_get_value :: YModel -> YDecl -> IO YBool

foreign import ccall "yices1_get_int_value"
    c_yices_get_int_value :: YModel -> YDecl -> Ptr CLong -> IO CInt

foreign import ccall "yices1_get_bitvector_value"
    c_yices_get_bitvector_value :: YModel -> YDecl -> CUInt -> Ptr CInt -> IO CInt

foreign import ccall "yices1_get_var_decl_from_name"
    c_yices_get_var_decl_from_name :: Ptr YContext -> CString -> IO YDecl

foreign import ccall "yices1_mk_var_from_decl"
    c_yices_mk_var_from_decl :: Ptr YContext -> YDecl -> IO YExpr

foreign import ccall "yices1_mk_function_update"
    c_yices_mk_function_update :: Ptr YContext -> YExpr -> Ptr YExpr -> CUInt -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_app"
    c_yices_mk_app :: Ptr YContext -> YExpr -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_true"
    c_yices_mk_true :: Ptr YContext -> IO YExpr

foreign import ccall "yices1_mk_false"
    c_yices_mk_false :: Ptr YContext -> IO YExpr

foreign import ccall "yices1_mk_not"
    c_yices_mk_not :: Ptr YContext -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_not"
    c_yices_mk_bv_not :: Ptr YContext -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_or"
    c_yices_mk_or :: Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_and"
    c_yices_mk_and :: Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_sum"
    c_yices_mk_sum :: Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_sub"
    c_yices_mk_sub :: Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_mul"
    c_yices_mk_mul :: Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_ite"
    c_yices_mk_ite :: Ptr YContext -> YExpr -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_eq"
    c_yices_mk_eq :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_lt"
    c_yices_mk_lt :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_le"
    c_yices_mk_le :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_gt"
    c_yices_mk_gt :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_ge"
    c_yices_mk_ge :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_add"
    c_yices_mk_bv_add :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_sub"
    c_yices_mk_bv_sub :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_mul"
    c_yices_mk_bv_mul :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_and"
    c_yices_mk_bv_and :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_or"
    c_yices_mk_bv_or :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_concat"
    c_yices_mk_bv_concat :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_sign_extend"
    c_yices_mk_bv_sign_extend :: Ptr YContext -> YExpr -> CUInt -> IO YExpr

foreign import ccall "yices1_mk_bv_extract"
    c_yices_mk_bv_extract :: Ptr YContext -> CUInt -> CUInt -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_lt"
    c_yices_mk_bv_lt :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_gt"
    c_yices_mk_bv_gt :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_le"
    c_yices_mk_bv_le :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_bv_ge"
    c_yices_mk_bv_ge :: Ptr YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices1_mk_num"
    c_yices_mk_num :: Ptr YContext -> CInt -> IO YExpr

foreign import ccall "yices1_mk_bv_constant"
    c_yices_mk_bv_constant :: Ptr YContext -> CUInt -> CULong -> IO YExpr

foreign import ccall "yices1_mk_bv_shift_left0"
    c_yices_mk_bv_shift_left0 :: Ptr YContext -> YExpr -> CUInt -> IO YExpr

toResult :: YBool -> S.Result
toResult n
    | n == yFalse = S.Unsatisfiable
    | n == yTrue  = S.Satisfiable
    | otherwise   = error "yices1 returned Unknown"

push :: Yices1 -> IO ()
push y = withy1 y c_yices_push

pop :: Yices1 -> IO ()
pop y = withy1 y c_yices_pop

y1assert :: Yices1 -> ExpH -> IO ()
y1assert = A.assert

y1fresh :: Yices1 -> Type -> IO Name
y1fresh y t = do
    nid <- readIORef (y1_nid y)
    modifyIORef' (y1_nid y) (+ 1)
    let nm = "f~" ++ show nid
        ty = case () of
                _ | t == boolT -> "bool"
                  | t == integerT -> "int"
                  | Just w <- de_bitT t -> "(bitvector " ++ show w ++ ")"
        cmd = "(define " ++ nm ++ " :: " ++ ty ++ ")"
    worked <- withCString cmd $ \str -> do
          withy1 y $ \yctx -> c_yices_parse_command yctx str
    if worked 
       then return $ name nm
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg ++ "\n when running command: \n" ++ cmd

check :: Yices1 -> IO S.Result
check y = do
    res <- withy1 y c_yices_check
    return $ toResult res

getIntegerValue :: Yices1 -> Name -> IO Integer
getIntegerValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString (unname nm) $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    x <- alloca $ \ptr -> do
        ir <- c_yices_get_int_value model decl ptr
        if ir == 1
            then peek ptr
            else return 0
    return (toInteger x)

getBoolValue :: Yices1 -> Name -> IO Bool
getBoolValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString (unname nm) $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    br <- c_yices_get_value model decl
    case br of
      _ | br == yTrue -> return True
      _ | br == yFalse -> return False
      _ | br == yUndef -> return False

getBitVectorValue :: Yices1 -> Integer -> Name -> IO Integer
getBitVectorValue y w nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString (unname nm) $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        ir <- c_yices_get_bitvector_value model decl (fromInteger w) ptr
        if ir == 1
            then peekArray (fromInteger w) ptr
            else return []
    return (bvInteger bits)

bvInteger :: [CInt] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)
        
yices1 :: IO S.Solver
yices1 = do
  c_yices_enable_type_checker True
  ptr <- c_yices_mk_context
  fp  <- F.newForeignPtr ptr (c_yices_del_context ptr)
  nid <- newIORef 0
  return $
    let y1 = Yices1 fp nid
    in S.Solver {
          S.push = push y1,
          S.pop = pop y1,
          S.fresh = y1fresh y1,
          S.assert = y1assert y1,
          S.check = check y1,
          S.getIntegerValue = getIntegerValue y1,
          S.getBoolValue = getBoolValue y1,
          S.getBitVectorValue = getBitVectorValue y1
       }

withy1 :: Yices1 -> (Ptr YContext -> IO a) -> IO a
withy1 (Yices1 fp _) f = withForeignPtr fp $ \ctx -> f ctx

instance AST Yices1 YExpr where
  fresh = y1fresh
  assert y p = withy1 y $ \ctx -> c_yices_assert ctx p
  
  literal y l
    | Just i <- de_integerL l = withy1 y $ \ctx ->
        c_yices_mk_num ctx (fromInteger i)   
    | Just bv <- de_bitL l = withy1 y $ \ctx -> 
        let w = fromInteger $ bv_width bv
            v = fromInteger $ bv_value bv
        in c_yices_mk_bv_constant ctx w v

  bool y True = withy1 y c_yices_mk_true
  bool y False = withy1 y c_yices_mk_false

  var y nm = withy1 y $ \ctx -> do
     decl <- withCString (unname nm) $ c_yices_get_var_decl_from_name ctx
     c_yices_mk_var_from_decl ctx decl

  ite y p a b = withy1 y $ \ctx -> c_yices_mk_ite ctx p a b

  unary = HT.table [(name "Smten.Bit.__prim_not_Bit", \y x ->
                        withy1 y $ \ctx -> c_yices_mk_bv_not ctx x)]

  binary =
   let bprim :: (Ptr YContext -> YExpr -> YExpr -> IO YExpr) ->
                Yices1 -> YExpr -> YExpr -> IO YExpr
       bprim f y a b = withy1 y $ \ctx -> f ctx a b

       baprim :: (Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr) ->
                Yices1 -> YExpr -> YExpr -> IO YExpr
       baprim f y a b = withy1 y $ \ctx ->
            withArray [a, b] $ \arr -> f ctx arr 2
   in HT.table [
    (name "Prelude.__prim_eq_Integer", bprim c_yices_mk_eq),
    (name "Prelude.__prim_add_Integer", baprim c_yices_mk_sum),
    (name "Prelude.__prim_sub_Integer", baprim c_yices_mk_sub),
    (name "Prelude.__prim_mul_Integer", baprim c_yices_mk_mul),
    (name "Prelude.__prim_lt_Integer", bprim c_yices_mk_lt),
    (name "Prelude.__prim_leq_Integer", bprim c_yices_mk_le),
    (name "Prelude.__prim_gt_Integer", bprim c_yices_mk_gt),
    (name "Prelude.__prim_geq_Integer", bprim c_yices_mk_ge),
    (name "Smten.Bit.__prim_eq_Bit", bprim c_yices_mk_eq),
    (name "Smten.Bit.__prim_lt_Bit", bprim c_yices_mk_bv_lt),
    (name "Smten.Bit.__prim_leq_Bit", bprim c_yices_mk_bv_le),
    (name "Smten.Bit.__prim_gt_Bit", bprim c_yices_mk_bv_gt),
    (name "Smten.Bit.__prim_geq_Bit", bprim c_yices_mk_bv_ge),
    (name "Smten.Bit.__prim_add_Bit", bprim c_yices_mk_bv_add),
    (name "Smten.Bit.__prim_sub_Bit", bprim c_yices_mk_bv_sub),
    (name "Smten.Bit.__prim_mul_Bit", bprim c_yices_mk_bv_mul),
    (name "Smten.Bit.__prim_or_Bit", bprim c_yices_mk_bv_or),
    (name "Smten.Bit.__prim_and_Bit", bprim c_yices_mk_bv_and),
    (name "Smten.Bit.__prim_concat_Bit", bprim c_yices_mk_bv_concat)
    ]

  zeroextend = error "TODO: yices1 zeroExtend"
  signextend y a n = withy1 y $ \ctx -> c_yices_mk_bv_sign_extend ctx a (fromInteger n)
  extract y x hi lo = withy1 y $ \ctx -> c_yices_mk_bv_extract ctx (fromInteger hi) (fromInteger lo) x


