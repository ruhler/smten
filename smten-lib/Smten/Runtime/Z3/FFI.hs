
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Raw FFI interface to z3.
module Smten.Runtime.Z3.FFI where

import Data.Typeable
import Foreign
import Foreign.C.String
import Foreign.C.Types

data Z3Config_
type Z3Config = Ptr Z3Config_

data Z3Context_
type Z3Context = Ptr Z3Context_

data Z3Sort_
type Z3Sort = Ptr Z3Sort_

data Z3Symbol_
type Z3Symbol = Ptr Z3Symbol_

data Z3Solver_
type Z3Solver = Ptr Z3Solver_

data Z3Expr_ deriving (Typeable)
type Z3Expr = Ptr Z3Expr_

data Z3Model_
type Z3Model = Ptr Z3Model_

data Z3Decl_
type Z3Decl = Ptr Z3Decl_

type Z3Bool = CInt

z3true :: Z3Bool
z3true = 1

z3false :: Z3Bool
z3false = 0

type Z3LBool = CInt

z3ltrue :: Z3LBool
z3ltrue = 1

z3lfalse :: Z3LBool
z3lfalse = -1

z3lundef :: Z3LBool
z3lundef = 0

foreign import ccall "z3_load"
    c_Z3_load :: IO CInt

foreign import ccall "z3_mk_bool_sort"
    c_Z3_mk_bool_sort :: Z3Context -> IO Z3Sort

foreign import ccall "z3_mk_int_sort"
    c_Z3_mk_int_sort :: Z3Context -> IO Z3Sort

foreign import ccall "z3_mk_bv_sort"
    c_Z3_mk_bv_sort :: Z3Context -> CUInt -> IO Z3Sort

foreign import ccall "z3_mk_string_symbol"
    c_Z3_mk_string_symbol :: Z3Context -> CString -> IO Z3Symbol

foreign import ccall "z3_mk_func_decl"
    c_Z3_mk_func_decl :: Z3Context -> Z3Symbol -> CUInt -> Ptr Z3Sort -> Z3Sort -> IO Z3Decl

foreign import ccall "z3_mk_app"
    c_Z3_mk_app :: Z3Context -> Z3Decl -> CUInt -> Ptr Z3Expr -> IO Z3Expr

foreign import ccall "z3_solver_get_model"
    c_Z3_solver_get_model :: Z3Context -> Z3Solver -> IO Z3Model

foreign import ccall "z3_model_eval"
    c_Z3_model_eval :: Z3Context -> Z3Model -> Z3Expr -> Z3Bool -> Ptr Z3Expr -> IO Z3Bool

foreign import ccall "z3_model_get_const_interp"
    c_Z3_model_get_const_interp :: Z3Context -> Z3Model -> Z3Decl -> IO Z3Expr

foreign import ccall "z3_get_bool_value"
    c_Z3_get_bool_value :: Z3Context -> Z3Expr -> IO Z3LBool

foreign import ccall "z3_get_numeral_string"
    c_Z3_get_numeral_string :: Z3Context -> Z3Expr -> IO CString

foreign import ccall "z3_solver_check"
    c_Z3_solver_check :: Z3Context -> Z3Solver -> IO Z3LBool

foreign import ccall "z3_del_context"
    c_Z3_del_context :: Z3Context -> IO ()

foreign import ccall "z3_solver_assert"
    c_Z3_solver_assert :: Z3Context -> Z3Solver -> Z3Expr -> IO ()

foreign import ccall "z3_mk_true"
    c_Z3_mk_true :: Z3Context -> IO Z3Expr

foreign import ccall "z3_mk_false"
    c_Z3_mk_false :: Z3Context -> IO Z3Expr

foreign import ccall "z3_mk_and"
    c_Z3_mk_and :: Z3Context -> CUInt -> Ptr Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_not"
    c_Z3_mk_not :: Z3Context -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_ite"
    c_Z3_mk_ite :: Z3Context -> Z3Expr -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_eq"
    c_Z3_mk_eq :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_le"
    c_Z3_mk_le :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_add"
    c_Z3_mk_add :: Z3Context -> CUInt -> Ptr Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_sub"
    c_Z3_mk_sub :: Z3Context -> CUInt -> Ptr Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_mul"
    c_Z3_mk_mul :: Z3Context -> CUInt -> Ptr Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvule"
    c_Z3_mk_bvule :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvadd"
    c_Z3_mk_bvadd :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvsub"
    c_Z3_mk_bvsub :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvmul"
    c_Z3_mk_bvmul :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvor"
    c_Z3_mk_bvor :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvand"
    c_Z3_mk_bvand :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_concat"
    c_Z3_mk_concat :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvshl"
    c_Z3_mk_bvshl :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvlshr"
    c_Z3_mk_bvlshr :: Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_bvnot"
    c_Z3_mk_bvnot :: Z3Context -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_sign_ext"
    c_Z3_mk_sign_ext :: Z3Context -> CUInt -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_extract"
    c_Z3_mk_extract :: Z3Context -> CUInt -> CUInt -> Z3Expr -> IO Z3Expr

foreign import ccall "z3_mk_context"
    c_Z3_mk_context :: Z3Config -> IO Z3Context

foreign import ccall "z3_mk_solver"
    c_Z3_mk_solver :: Z3Context -> IO Z3Solver

foreign import ccall "z3_solver_inc_ref"
    c_Z3_solver_inc_ref :: Z3Context -> Z3Solver -> IO ()

foreign import ccall "z3_solver_dec_ref"
    c_Z3_solver_dec_ref :: Z3Context -> Z3Solver -> IO ()

foreign import ccall "z3_mk_config"
    c_Z3_mk_config :: IO Z3Config

foreign import ccall "z3_del_config"
    c_Z3_del_config :: Z3Config -> IO ()

foreign import ccall "z3_set_param_value"
    c_Z3_set_param_value :: Z3Config -> CString -> CString -> IO ()

foreign import ccall "z3_mk_numeral"
    c_Z3_mk_numeral :: Z3Context -> CString -> Z3Sort -> IO Z3Expr

foreign import ccall "z3_inc_ref"
    c_Z3_inc_ref :: Z3Context -> Z3Expr -> IO ()

