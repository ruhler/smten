
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-- | Backend for the Yices1 Solver
--
-- NOTE: This assumes all the symbols from the yices1 library starting with
-- yices_ have been renamed to yices1_. This is so yices1 and yices2 can
-- coexist.
module Smten.Compiled.Smten.Symbolic.Solver.Yices1 (yices1) where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Smten.Runtime.Yices1FFI
import qualified Smten.Runtime.Types as S
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Yices1 = Yices1 {
    y1_ctx :: Ptr YContext
}

declare :: String -> Yices1 -> String -> IO ()
declare ty y nm = do
    let cmd = "(define " ++ nm ++ " :: " ++ ty ++ ")"
    worked <- withCString cmd $ \str -> do
          withy1 y $ \yctx -> c_yices_parse_command yctx str
    if worked 
       then return ()
       else do
          cstr <- c_yices_get_last_error_message
          msg <- peekCString cstr
          fail $ show msg ++ "\n when running command: \n" ++ cmd

withy1 :: Yices1 -> (Ptr YContext -> IO a) -> IO a
withy1 y f = f (y1_ctx y)

bvInteger :: [CInt] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

uprim :: (Ptr YContext -> YExpr -> IO YExpr) ->
         Yices1 -> YExpr -> IO YExpr
uprim f y a = withy1 y $ \ctx -> f ctx a

bprim :: (Ptr YContext -> YExpr -> YExpr -> IO YExpr) ->
         Yices1 -> YExpr -> YExpr -> IO YExpr
bprim f y a b = withy1 y $ \ctx -> f ctx a b

baprim :: (Ptr YContext -> Ptr YExpr -> CUInt -> IO YExpr) ->
          Yices1 -> YExpr -> YExpr -> IO YExpr
baprim f y a b = withy1 y $ \ctx -> withArray [a, b] $ \arr -> f ctx arr 2

instance SolverAST Yices1 YExpr where
  declare y ty nm = do
      let tynm = case ty of
                    S.BoolT -> "bool"
                    S.IntegerT -> "int"
                    S.BitT w -> "(bitvector " ++ show w ++ ")"
          cmd = "(define " ++ nm ++ " :: " ++ tynm ++ ")"
      worked <- withCString cmd $ \str -> do
            withy1 y $ \yctx -> c_yices_parse_command yctx str
      if worked 
         then return ()
         else do
            cstr <- c_yices_get_last_error_message
            msg <- peekCString cstr
            fail $ show msg ++ "\n when running command: \n" ++ cmd

  getBoolValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    br <- c_yices_get_value model decl
    case br of
      _ | br == yTrue -> return True
      _ | br == yFalse -> return False
      _ | br == yUndef -> return False

  getIntegerValue y nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    x <- alloca $ \ptr -> do
        ir <- c_yices_get_int_value model decl ptr
        if ir == 1
            then peek ptr
            else return 0
    return (toInteger x)

  getBitVectorValue y w nm = do
    model <- withy1 y c_yices_get_model 
    decl <- withCString nm $ \str ->
                withy1 y $ \yctx -> c_yices_get_var_decl_from_name yctx str
    bits <- allocaArray (fromInteger w) $ \ptr -> do
        ir <- c_yices_get_bitvector_value model decl (fromInteger w) ptr
        if ir == 1
            then peekArray (fromInteger w) ptr
            else return []
    return (bvInteger bits)


  check y = do
    res <- withy1 y c_yices_check
    return $ toResult res

  assert y p = withy1 y $ \ctx -> c_yices_assert ctx p
  bool y True = withy1 y c_yices_mk_true
  bool y False = withy1 y c_yices_mk_false
  integer y i = withy1 y $ \ctx -> c_yices_mk_num ctx (fromInteger i)
  bit y w v = withy1 y $ \ctx ->
        let w' = fromInteger w
            v' = fromInteger v
        in c_yices_mk_bv_constant ctx w' v'

  var y nm = withy1 y $ \ctx -> do
     decl <- withCString nm $ c_yices_get_var_decl_from_name ctx
     c_yices_mk_var_from_decl ctx decl

  and_bool = baprim c_yices_mk_and
  not_bool = uprim c_yices_mk_not

  ite_bool y p a b = withy1 y $ \ctx -> c_yices_mk_ite ctx p a b
  ite_integer y p a b = withy1 y $ \ctx -> c_yices_mk_ite ctx p a b
  ite_bit y p a b = withy1 y $ \ctx -> c_yices_mk_ite ctx p a b

  eq_integer = bprim c_yices_mk_eq
  leq_integer = bprim c_yices_mk_le
  add_integer = baprim c_yices_mk_sum
  sub_integer = baprim c_yices_mk_sub

  eq_bit = bprim c_yices_mk_eq
  leq_bit = bprim c_yices_mk_bv_le
  add_bit = bprim c_yices_mk_bv_add
  sub_bit = bprim c_yices_mk_bv_sub
  mul_bit = bprim c_yices_mk_bv_mul
  or_bit = bprim c_yices_mk_bv_or
  and_bit = bprim c_yices_mk_bv_and
  concat_bit = bprim c_yices_mk_bv_concat
  shl_bit = error "TODO: shl_bit for Yices1"
  lshr_bit = error "TODO: lshr_bit for Yices1"
  not_bit = uprim c_yices_mk_bv_not
--  sign_extend_bit y n a = withy1 y $ \ctx ->
--     c_yices_mk_bv_sign_extend ctx a (fromInteger n)
--  extract_bit y hi lo x = withy1 y $ \ctx ->
--     c_yices_mk_bv_extract ctx (fromInteger hi) (fromInteger lo) x

-- TODO: does this leak solvers?
yices1 :: Solver
yices1 = do
  ptr <- c_yices_mk_context
  return (solverInstFromAST $ Yices1 ptr)

