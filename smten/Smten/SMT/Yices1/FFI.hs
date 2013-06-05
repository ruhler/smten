
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw FFI interface to yices1.
module Smten.SMT.Yices1.FFI where

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Smten.SMT.Solver(Result(..))

data YContext

data YModel_
type YModel = Ptr YModel_

data YDecl_
type YDecl = Ptr YDecl_

data YExpr_
type YExpr = Ptr YExpr_

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

toResult :: YBool -> Result
toResult n
    | n == yFalse = Unsatisfiable
    | n == yTrue  = Satisfiable
    | otherwise   = error "yices1 returned Unknown"

