
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Raw FFI interface to yices1.
module Smten.Runtime.Yices1.FFI where

import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Smten.Runtime.Result

data YType_
type YType = Ptr YType_

data YContext_
type YContext = Ptr YContext_

data YModel_
type YModel = Ptr YModel_

data YDecl_
type YDecl = Ptr YDecl_

data YExpr_ deriving (Typeable)
type YExpr = Ptr YExpr_

type YBool = CInt

yFalse, yTrue, yUndef :: YBool
yFalse = -1
yTrue  = 1
yUndef = 0

foreign import ccall "yices_mk_context"
    c_yices_mk_context  :: IO YContext

foreign import ccall "yices_del_context"
    c_yices_del_context :: YContext -> IO ()

foreign import ccall "yices_parse_command"
    c_yices_parse_command :: YContext -> CString -> IO Bool

foreign import ccall "yices_check"
    c_yices_check :: YContext -> IO YBool

foreign import ccall "yices_assert"
    c_yices_assert :: YContext -> YExpr -> IO ()

foreign import ccall "yices_get_model"
    c_yices_get_model :: YContext -> IO YModel

foreign import ccall "yices_get_last_error_message"
    c_yices_get_last_error_message :: IO CString

foreign import ccall "yices_get_value"
    c_yices_get_value :: YModel -> YDecl -> IO YBool

foreign import ccall "yices_get_int_value"
    c_yices_get_int_value :: YModel -> YDecl -> Ptr CLong -> IO CInt

foreign import ccall "yices_get_bitvector_value"
    c_yices_get_bitvector_value :: YModel -> YDecl -> CUInt -> Ptr CInt -> IO CInt

foreign import ccall "yices_get_var_decl_from_name"
    c_yices_get_var_decl_from_name :: YContext -> CString -> IO YDecl

foreign import ccall "yices_mk_type"
    c_yices_mk_type :: YContext -> CString -> IO YType

foreign import ccall "yices_mk_bitvector_type"
    c_yices_mk_bitvector_type :: YContext -> CUInt -> IO YType

foreign import ccall "yices_mk_var_decl"
    c_yices_mk_var_decl :: YContext -> CString -> YType -> IO YDecl

foreign import ccall "yices_mk_var_from_decl"
    c_yices_mk_var_from_decl :: YContext -> YDecl -> IO YExpr

foreign import ccall "yices_mk_true"
    c_yices_mk_true :: YContext -> IO YExpr

foreign import ccall "yices_mk_false"
    c_yices_mk_false :: YContext -> IO YExpr

foreign import ccall "yices_mk_not"
    c_yices_mk_not :: YContext -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_not"
    c_yices_mk_bv_not :: YContext -> YExpr -> IO YExpr

foreign import ccall "yices_mk_and"
    c_yices_mk_and :: YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_or"
    c_yices_mk_or :: YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_sum"
    c_yices_mk_sum :: YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_sub"
    c_yices_mk_sub :: YContext -> Ptr YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_ite"
    c_yices_mk_ite :: YContext -> YExpr -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_eq"
    c_yices_mk_eq :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_le"
    c_yices_mk_le :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_add"
    c_yices_mk_bv_add :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_sub"
    c_yices_mk_bv_sub :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_mul"
    c_yices_mk_bv_mul :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_and"
    c_yices_mk_bv_and :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_or"
    c_yices_mk_bv_or :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_concat"
    c_yices_mk_bv_concat :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_sign_extend"
    c_yices_mk_bv_sign_extend :: YContext -> YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_bv_extract"
    c_yices_mk_bv_extract :: YContext -> CUInt -> CUInt -> YExpr -> IO YExpr

foreign import ccall "yices_mk_bv_le"
    c_yices_mk_bv_le :: YContext -> YExpr -> YExpr -> IO YExpr

foreign import ccall "yices_mk_num"
    c_yices_mk_num :: YContext -> CInt -> IO YExpr

foreign import ccall "yices_mk_bv_constant"
    c_yices_mk_bv_constant :: YContext -> CUInt -> CULong -> IO YExpr

foreign import ccall "yices_mk_bv_constant_from_array"
    c_yices_mk_bv_constant_from_array :: YContext -> CUInt -> Ptr CInt -> IO YExpr

foreign import ccall "yices_mk_bv_shift_left0"
    c_yices_mk_bv_shift_left0 :: YContext -> YExpr -> CUInt -> IO YExpr

foreign import ccall "yices_mk_bv_shift_right0"
    c_yices_mk_bv_shift_right0 :: YContext -> YExpr -> CUInt -> IO YExpr

toResult :: YBool -> Result
toResult n
    | n == yFalse = Unsat
    | n == yTrue  = Sat
    | otherwise   = error "y1 returned Unknown"

