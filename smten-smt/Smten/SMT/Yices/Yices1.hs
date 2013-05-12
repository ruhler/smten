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

import qualified Data.Map as Map

import qualified Smten.SMT.Solver as S
import Smten.SMT.Syntax
import qualified Smten.SMT.Yices.Concrete as YC

data YContext

data YModel_
type YModel = Ptr YModel_

data YDecl_
type YDecl = Ptr YDecl_

data YExpr_
type YExpr = Ptr YExpr_

data Yices1 = Yices1 (ForeignPtr YContext)

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

builtin :: [String]
builtin = [
    "true", "false", "if", "ite", "=", "/=", "distinct",
    "or", "and", "not", "xor", "<=>", "=>", "mk-tuple",
    "select", "tuple-update", "update",  "forall",  "exists",  "let",
    "+", "-", "*", "/", "^",
    "<", "<=", ">", ">=",
    "mk-bv", "bv-add", "bv-sub", "bv-mul", "bv-neg", "bv-pow",
    "bv-not", "bv-and", "bv-or", "bv-xor", "bv-nand", "bv-nor", "bv-xnor",
    "bv-shift-left0", "bv-shift-left1", "bv-shift-right0", "bv-shift-right1",
    "bv-ashift-right", "bv-rotate-left", "bv-rotate-right",
    "bv-extract", "bv-concat", "bv-repeat",
    "bv-sign-extend", "bv-zero-extend", "bv-ge", "bv-gt", "bv-le", "bv-lt",
    "bv-sge", "bv-sgt", "bv-sle", "bv-slt", "bv-shl", "bv-lshr", "bv-ashr",
    "bv-div", "bv-rem", "bv-sdiv", "bv-srem", "bv-smod",
    "bv-redor", "bv-redand", "bv-comp"
    ]
                          

toResult :: YBool -> S.Result
toResult n
    | n == yFalse = S.Unsatisfiable
    | n == yTrue  = S.Satisfiable
    | otherwise   = error "yices1 returned Unknown"

push :: Yices1 -> IO ()
push (Yices1 fp) = withForeignPtr fp c_yices_push

pop :: Yices1 -> IO ()
pop (Yices1 fp) = withForeignPtr fp c_yices_pop

assert :: Yices1 -> Expression -> IO ()
assert y@(Yices1 fp) p = do
    p' <- yexpr y p
    withForeignPtr fp $ \ctx -> c_yices_assert ctx p'

declare :: Yices1 -> Symbol -> Type -> IO ()
declare (Yices1 fp) s t = do
    worked <- withCString (YC.concrete (Declare s t)) $ \str -> do
          withForeignPtr fp $ \yctx ->
            c_yices_parse_command yctx str
    if worked 
       then return ()
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg
                    ++ "\n when running command: \n" 
                    ++ YC.pretty (Declare s t)

check :: Yices1 -> IO S.Result
check (Yices1 fp) = do
    res <- withForeignPtr fp c_yices_check
    return $ toResult res

getIntegerValue :: Yices1 -> String -> IO Integer
getIntegerValue (Yices1 fp) nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
    x <- alloca $ \ptr -> do
        ir <- c_yices_get_int_value model decl ptr
        if ir == 1
            then peek ptr
            else return 0
    return (toInteger x)

getBoolValue :: Yices1 -> String -> IO Bool
getBoolValue (Yices1 fp) nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
    br <- c_yices_get_value model decl
    case br of
      _ | br == yTrue -> return True
      _ | br == yFalse -> return False
      _ | br == yUndef -> return False

getBitVectorValue :: Yices1 -> Integer -> String -> IO Integer
getBitVectorValue (Yices1 fp) w nm = do
    model <- withForeignPtr fp c_yices_get_model 
    decl <- withCString nm $ \str ->
                withForeignPtr fp $ \yctx ->
                    c_yices_get_var_decl_from_name yctx str
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
  return $
    let y1 = Yices1 fp
    in S.Solver {
          S.push = push y1,
          S.pop = pop y1,
          S.declare = declare y1,
          S.assert = assert y1,
          S.check = check y1,
          S.getIntegerValue = getIntegerValue y1,
          S.getBoolValue = getBoolValue y1,
          S.getBitVectorValue = getBitVectorValue y1
       }

yexpr :: Yices1 -> Expression -> IO YExpr
yexpr y = yexprS y Map.empty

dobinop :: Yices1 -> Map.Map String YExpr
           -> Expression -> Expression
           -> (Ptr YContext -> YExpr -> YExpr -> IO YExpr)
           -> IO YExpr
dobinop y@(Yices1 fp) s a b f = do
    a' <- yexprS y s a
    b' <- yexprS y s b
    withForeignPtr fp $ \ctx -> f ctx a' b'

domanyop :: Yices1 -> Map.Map String YExpr
           -> [Expression] 
           -> (Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr)
           -> IO YExpr
domanyop y@(Yices1 fp) s xs f = do
    xs' <- mapM (yexprS y s) xs
    withForeignPtr fp $ \ctx ->
        withArray xs' $ \arr -> f ctx arr (fromIntegral $ length xs')

yexprS :: Yices1 -> Map.Map String YExpr -> Expression -> IO YExpr
yexprS y@(Yices1 fp) s e
  | Just True <- de_boolE e = withForeignPtr fp c_yices_mk_true
  | Just False <- de_boolE e = withForeignPtr fp c_yices_mk_false
  | Just i <- de_integerE e = withForeignPtr fp $ \ctx ->
        c_yices_mk_num ctx (fromInteger i)
  | Just a <- de_notE e = do
        a' <- yexprS y s a
        withForeignPtr fp $ \ctx -> c_yices_mk_not ctx a'
  | Just a <- de_bvnotE e = do
        a' <- yexprS y s a
        withForeignPtr fp $ \ctx -> c_yices_mk_bv_not ctx a'
  | Just (a, b) <- de_eqE e = dobinop y s a b c_yices_mk_eq
  | Just (a, b) <- de_ltE e = dobinop y s a b c_yices_mk_lt
  | Just (a, b) <- de_leqE e = dobinop y s a b c_yices_mk_le
  | Just (a, b) <- de_gtE e = dobinop y s a b c_yices_mk_gt
  | Just (a, b) <- de_geqE e = dobinop y s a b c_yices_mk_ge
  | Just (a, b) <- de_addE e = domanyop y s [a, b] c_yices_mk_sum
  | Just (a, b) <- de_subE e = domanyop y s [a, b] c_yices_mk_sub
  | Just (a, b) <- de_mulE e = domanyop y s [a, b] c_yices_mk_mul
  | Just (a, b) <- de_bvaddE e = dobinop y s a b c_yices_mk_bv_add
  | Just (a, b) <- de_bvsubE e = dobinop y s a b c_yices_mk_bv_sub
  | Just (a, b) <- de_bvandE e = dobinop y s a b c_yices_mk_bv_and
  | Just (a, b) <- de_bvorE e = dobinop y s a b c_yices_mk_bv_or
  | Just (a, b) <- de_bvconcatE e = dobinop y s a b c_yices_mk_bv_concat
  | Just (a, b) <- de_bvltE e = dobinop y s a b c_yices_mk_bv_lt
  | Just (a, b) <- de_bvleqE e = dobinop y s a b c_yices_mk_bv_le
  | Just (a, b) <- de_bvgtE e = dobinop y s a b c_yices_mk_bv_gt
  | Just (a, b) <- de_bvgeqE e = dobinop y s a b c_yices_mk_bv_ge
  | Just xs <- de_orE e = domanyop y s xs c_yices_mk_or
  | Just xs <- de_andE e = domanyop y s xs c_yices_mk_and
  | Just (p, a, b) <- de_ifE e = do
      p' <- yexprS y s p
      a' <- yexprS y s a
      b' <- yexprS y s b
      withForeignPtr fp $ \ctx -> c_yices_mk_ite ctx p' a' b'
  | Just (bs, v) <- de_letE e =
      let mkvar :: (String, Expression) -> IO (String, YExpr)
          mkvar (nm, e) = do
            et <- yexprS y s e
            return (nm, et)
      in do
        vars <- mapM mkvar bs
        yexprS y (Map.union s (Map.fromList vars)) v
  | Just (w, v) <- de_mkbvE e = withForeignPtr fp $ \ctx ->
        c_yices_mk_bv_constant ctx (fromInteger w) (fromInteger v)
  | Just (a, n) <- de_bvsignExtendE e = do
        a' <- yexprS y s a
        withForeignPtr fp $ \ctx -> c_yices_mk_bv_sign_extend ctx a' (fromInteger n)
  | Just (a, b) <- de_bvshlE e
  , Just bv <- de_integerE b = do
        a' <- yexprS y s a
        withForeignPtr fp $ \ctx ->
            c_yices_mk_bv_shift_left0 ctx a' (fromInteger bv)
  | Just (a, b) <- de_bvshlE e = do
      -- TODO: This won't work if b contains local variable references!
      yexprbystr y e
  | VarE nm <- e =  
      case Map.lookup nm s of
        Nothing -> withForeignPtr fp $ \ctx -> do
            decl <- withCString nm $ c_yices_get_var_decl_from_name ctx
            c_yices_mk_var_from_decl ctx decl
        Just t -> return t
  | AppE (VarE f) _ <- e
  , f `elem` builtin = do
        error $ "TODO: yexpr builtin " ++ YC.pretty e
  | AppE f xs <- e = do
       f' <- yexprS y s f
       xs' <- mapM (yexprS y s) xs
       withForeignPtr fp $ \ctx ->
          withArray xs' $ \arr ->
              c_yices_mk_app ctx f' arr (fromIntegral $ length xs')
  | otherwise = error $ "TODO: yexpr: " ++ YC.pretty e

yexprbystr :: Yices1 -> Expression -> IO YExpr
yexprbystr (Yices1 fp) e =
    withCString (YC.concrete e) $ \str ->
      withForeignPtr fp $ \ctx ->
        c_yices_parse_expression ctx str

