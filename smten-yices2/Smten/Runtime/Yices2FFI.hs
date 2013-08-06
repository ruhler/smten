
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw FFI interface to yices2.
module Smten.Runtime.Yices2FFI (
    YContext, YModel, YType, YTerm, YContextConfig,
    YParam, YSMTStatus,
    fromYSMTStatus,
    c_yices_init,
    c_yices_exit,
    c_yices_print_error,
    c_yices_new_scalar_type,
    c_yices_constant,
    c_yices_new_uninterpreted_type,
    c_yices_new_uninterpreted_term,
    c_yices_set_type_name,
    c_yices_set_term_name,
    c_yices_get_type_by_name,
    c_yices_get_term_by_name,
    c_yices_parse_type,
    c_yices_parse_term,
    c_yices_tuple_type,
    c_yices_function_type,
    c_yices_bv_type,
    c_yices_int_type,
    c_yices_bool_type,
    c_yices_real_type,
    
    c_yices_true,
    c_yices_false,
    c_yices_int64,
    c_yices_not,
    c_yices_eq,
    c_yices_tuple,
    c_yices_tuple_update,
    c_yices_select,
    c_yices_application,
    c_yices_update,
    c_yices_ite,
    c_yices_arith_lt_atom,
    c_yices_arith_leq_atom,
    c_yices_arith_gt_atom,
    c_yices_arith_geq_atom,
    c_yices_add,
    c_yices_sub,
    c_yices_mul,
    c_yices_or2,
    c_yices_and2,
    c_yices_and,
    c_yices_or,
    c_yices_xor2,

    c_yices_bvconst_uint64,
    c_yices_parse_bvbin,
    c_yices_bvadd,
    c_yices_bvsub,
    c_yices_bvmul,
    c_yices_bvand,
    c_yices_bvor,
    c_yices_bvnot,
    c_yices_zero_extend,
    c_yices_sign_extend,
    c_yices_bvshl,
    c_yices_bvlshr,
    c_yices_shift_left0,
    c_yices_shift_right0,
    c_yices_bvextract,
    c_yices_bvconcat,
    c_yices_new_context,
    c_yices_free_context,
    c_yices_assert_formula,
    c_yices_check_context,
    c_yices_push,
    c_yices_pop,
    c_yices_get_model,
    c_yices_free_model,
    c_yices_get_bool_value,
    c_yices_get_int64_value,
    c_yices_get_bv_value,
    c_yices_print_model,
    c_yices_bvlt_atom,
    c_yices_bvle_atom,
    c_yices_bvgt_atom,
    c_yices_bvge_atom,
    ) where

import Data.Int

import Foreign
import Foreign.C.String
import Foreign.C.Types

import Smten.Runtime.Result

data YContext
data YModel
data YContextConfig
data YParam
type YType = Int32
type YTerm = Int32
type YSMTStatus = CInt

fromYSMTStatus :: YSMTStatus -> Result
fromYSMTStatus 3 = Sat
fromYSMTStatus 4 = Unsat
fromYSMTStatus _ = error "yices2 returned Unknown"

foreign import ccall unsafe "yices_init"
    c_yices_init :: IO ()

foreign import ccall unsafe "yices_exit"
    c_yices_exit :: IO ()

foreign import ccall unsafe "yices_print_error"
    c_yices_print_error :: Ptr CFile -> IO ()

foreign import ccall unsafe "yices_new_scalar_type"
    c_yices_new_scalar_type :: Int32 -> IO YType

foreign import ccall unsafe "yices_constant"
    c_yices_constant :: YType -> Int32 -> IO YTerm

foreign import ccall unsafe "yices_new_uninterpreted_type"
    c_yices_new_uninterpreted_type :: IO YType

foreign import ccall unsafe "yices_new_uninterpreted_term"
    c_yices_new_uninterpreted_term :: YType -> IO YTerm

foreign import ccall unsafe "yices_set_type_name"
    c_yices_set_type_name :: YType -> CString -> IO ()

foreign import ccall unsafe "yices_set_term_name"
    c_yices_set_term_name :: YTerm -> CString -> IO ()

foreign import ccall unsafe "yices_get_type_by_name"
    c_yices_get_type_by_name :: CString -> IO YType

foreign import ccall unsafe "yices_get_term_by_name"
    c_yices_get_term_by_name :: CString -> IO YTerm

foreign import ccall unsafe "yices_parse_type"
    c_yices_parse_type :: CString -> IO YType

foreign import ccall unsafe "yices_parse_term"
    c_yices_parse_term :: CString -> IO YTerm

foreign import ccall unsafe "yices_tuple_type"
    c_yices_tuple_type :: Word32 -> Ptr YType -> IO YType
    
foreign import ccall unsafe "yices_function_type"
    c_yices_function_type :: Word32 -> Ptr YType -> YType -> IO YType  

foreign import ccall unsafe "yices_bv_type"
    c_yices_bv_type :: Word32 -> IO YType

foreign import ccall unsafe "yices_int_type"
    c_yices_int_type :: IO YType

foreign import ccall unsafe "yices_bool_type"
    c_yices_bool_type :: IO YType

foreign import ccall unsafe "yices_real_type"
    c_yices_real_type :: IO YType

foreign import ccall unsafe "yices_true"
    c_yices_true ::  IO YTerm

foreign import ccall unsafe "yices_false"
    c_yices_false ::  IO YTerm

foreign import ccall unsafe "yices_int64"
    c_yices_int64 :: Int64 ->  IO YTerm

foreign import ccall unsafe "yices_not"
    c_yices_not ::  YTerm -> IO YTerm

foreign import ccall unsafe "yices_eq"
    c_yices_eq :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_tuple"
    c_yices_tuple :: Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall unsafe "yices_tuple_update"
    c_yices_tuple_update :: YTerm -> Word32 -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_select"
    c_yices_select :: Word32 -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_application"
    c_yices_application :: YTerm -> Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall unsafe "yices_update"
    c_yices_update :: YTerm -> Word32 -> Ptr YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_ite"
    c_yices_ite :: YTerm -> YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_arith_lt_atom"
    c_yices_arith_lt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_arith_leq_atom"
    c_yices_arith_leq_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_arith_gt_atom"
    c_yices_arith_gt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_arith_geq_atom"
    c_yices_arith_geq_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_add"
    c_yices_add :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_sub"
    c_yices_sub :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_mul"
    c_yices_mul :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_or2"
    c_yices_or2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_and2"
    c_yices_and2 :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_and"
    c_yices_and :: Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall unsafe "yices_or"
    c_yices_or :: Word32 -> Ptr YTerm -> IO YTerm

foreign import ccall unsafe "yices_xor2"
    c_yices_xor2 :: YTerm -> YTerm -> IO YTerm


foreign import ccall unsafe "yices_bvconst_uint64"
    c_yices_bvconst_uint64 :: Word32 -> Word64 -> IO YTerm

foreign import ccall unsafe "yices_parse_bvbin"
    c_yices_parse_bvbin :: CString -> IO YTerm

foreign import ccall unsafe "yices_bvadd"
    c_yices_bvadd :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvsub"
    c_yices_bvsub :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvmul"
    c_yices_bvmul :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvand"
    c_yices_bvand :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvor"
    c_yices_bvor :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvnot"
    c_yices_bvnot :: YTerm -> IO YTerm

foreign import ccall unsafe "yices_zero_extend"
    c_yices_zero_extend :: YTerm -> Word32 -> IO YTerm

foreign import ccall unsafe "yices_sign_extend"
    c_yices_sign_extend :: YTerm -> Word32 -> IO YTerm

foreign import ccall unsafe "yices_bvshl"
    c_yices_bvshl :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvlshr"
    c_yices_bvlshr :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_shift_left0"
    c_yices_shift_left0 :: YTerm -> Word32 -> IO YTerm

foreign import ccall unsafe "yices_shift_right0"
    c_yices_shift_right0 :: YTerm -> Word32 -> IO YTerm

foreign import ccall unsafe "yices_bvextract"
    c_yices_bvextract :: YTerm -> Word32 -> Word32 -> IO YTerm

foreign import ccall unsafe "yices_bvconcat"
    c_yices_bvconcat :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_new_context"
    c_yices_new_context :: Ptr YContextConfig -> IO (Ptr YContext)

foreign import ccall unsafe "yices_free_context"
    c_yices_free_context :: Ptr YContext -> IO ()

foreign import ccall unsafe "yices_assert_formula"
    c_yices_assert_formula :: Ptr YContext -> YTerm -> IO ()

foreign import ccall unsafe "yices_check_context"
    c_yices_check_context :: Ptr YContext -> Ptr YParam -> IO YSMTStatus

foreign import ccall unsafe "yices_push"
    c_yices_push :: Ptr YContext -> IO ()

foreign import ccall unsafe "yices_pop"
    c_yices_pop :: Ptr YContext -> IO ()

foreign import ccall unsafe "yices_get_model"
    c_yices_get_model :: Ptr YContext -> Int32 -> IO (Ptr YModel)

foreign import ccall unsafe "yices_free_model"
    c_yices_free_model :: Ptr YModel -> IO ()

foreign import ccall unsafe "yices_get_bool_value"
    c_yices_get_bool_value :: Ptr YModel -> YTerm -> Ptr Int32 -> IO Int32

foreign import ccall unsafe "yices_get_int64_value"
    c_yices_get_int64_value :: Ptr YModel -> YTerm -> Ptr Int64 -> IO Int32

foreign import ccall unsafe "yices_get_bv_value"
    c_yices_get_bv_value :: Ptr YModel -> YTerm -> Ptr Int32 -> IO Int32

foreign import ccall unsafe "yices_print_model"
    c_yices_print_model :: Ptr CFile -> Ptr YModel -> IO ()


foreign import ccall unsafe "yices_bvlt_atom"
    c_yices_bvlt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvle_atom"
    c_yices_bvle_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvgt_atom"
    c_yices_bvgt_atom :: YTerm -> YTerm -> IO YTerm

foreign import ccall unsafe "yices_bvge_atom"
    c_yices_bvge_atom :: YTerm -> YTerm -> IO YTerm
