
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Backend for the Z3 Solver
module Smten.Compiled.Smten.Symbolic.Solver.Z3 (z3) where

import Foreign hiding (bit)
import Foreign.C.String
import Foreign.C.Types

import Data.Functor
import Data.Maybe
import qualified Data.HashTable.IO as H

import Smten.Runtime.Bit
import Smten.Runtime.Build
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.Model
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.Z3.FFI

type VarMap = H.BasicHashTable FreeID Z3Decl

data Z3 = Z3 {
    z3_ctx :: Z3Context,
    z3_slv :: Z3Solver,
    z3_vars :: VarMap
}

withz3c :: Z3 -> (Z3Context -> IO a) -> IO a
withz3c z f = f (z3_ctx z)

withz3s :: Z3 -> (Z3Context -> Z3Solver -> IO a) -> IO a
withz3s z f = f (z3_ctx z) (z3_slv z)

uprim :: (Z3Context -> Z3Expr -> IO Z3Expr) -> Z3 -> Z3Expr -> IO Z3Expr
uprim f z a = withz3c z $ \ctx -> f ctx a

bprim :: (Z3Context -> Z3Expr -> Z3Expr -> IO Z3Expr) ->
         Z3 -> Z3Expr -> Z3Expr -> IO Z3Expr
bprim f z a b = withz3c z $ \ctx -> f ctx a b

baprim :: (Z3Context -> CUInt -> Ptr Z3Expr -> IO Z3Expr) ->
          Z3 -> Z3Expr -> Z3Expr -> IO Z3Expr
baprim f z a b = withz3c z $ \ctx -> withArray [a, b] $ \arr -> f ctx 2 arr

instance SolverAST Z3 Z3Expr Z3Expr Z3Expr where
  declare_bool z nm = withz3c z $ \ctx -> do
      sort <- c_Z3_mk_bool_sort ctx
      snm <- withCString (freenm nm) $ c_Z3_mk_string_symbol ctx
      decl <- c_Z3_mk_func_decl ctx snm 0 nullPtr sort
      H.insert (z3_vars z) nm decl

  declare_integer z nm = withz3c z $ \ctx -> do
      sort <- c_Z3_mk_int_sort ctx
      snm <- withCString (freenm nm) $ c_Z3_mk_string_symbol ctx
      decl <- c_Z3_mk_func_decl ctx snm 0 nullPtr sort
      H.insert (z3_vars z) nm decl

  declare_bit z w nm = withz3c z $ \ctx -> do
      sort <- c_Z3_mk_bv_sort ctx (fromInteger w)
      snm <- withCString (freenm nm) $ c_Z3_mk_string_symbol ctx
      decl <- c_Z3_mk_func_decl ctx snm 0 nullPtr sort
      H.insert (z3_vars z) nm decl

  getBoolValue z nm = do
    model <- withz3s z c_Z3_solver_get_model
    getBoolValueWithModel z nm model
    
  getIntegerValue z nm = do
    model <- withz3s z c_Z3_solver_get_model
    getIntegerValueWithModel z nm model

  getBitVectorValue z w nm = do
    model <- withz3s z c_Z3_solver_get_model
    getBitVectorValueWithModel z w nm model

  getModel z vars = do
    model <- withz3s z c_Z3_solver_get_model
    let getv (nm, BoolT) = BoolA <$> getBoolValueWithModel z nm model
        getv (nm, IntegerT) = IntegerA <$> getIntegerValueWithModel z nm model
        getv (nm, BitT w) = do
           b <- getBitVectorValueWithModel z w nm model
           return (BitA $ bv_make w b)
    mapM getv vars

  check z = do
    res <- withz3s z c_Z3_solver_check
    case res of
        _ | res == z3lfalse -> return Unsat
          | res == z3ltrue -> return Sat
          | otherwise -> error "Z3 check returned unknown"

  cleanup z = withz3s z $ \ctx slv -> do
     c_Z3_solver_dec_ref ctx slv
     c_Z3_del_context ctx

  assert z p = withz3s z $ \ctx slv -> c_Z3_solver_assert ctx slv p
  bool z True = withz3c z c_Z3_mk_true
  bool z False = withz3c z c_Z3_mk_false
  integer z i = withz3c z $ \ctx -> do
    sort <- c_Z3_mk_int_sort ctx
    withCString (show i) $ \cstr -> c_Z3_mk_numeral ctx cstr sort
    
  bit z w v = withz3c z $ \ctx -> do
    sort <- c_Z3_mk_bv_sort ctx (fromInteger w)
    withCString (show v) $ \cstr -> c_Z3_mk_numeral ctx cstr sort

  var_bool z nm = do
     decl <- fromMaybe (error "Z3 var not found") <$> H.lookup (z3_vars z) nm
     withz3c z $ \ctx -> c_Z3_mk_app ctx decl 0 nullPtr

  var_integer z nm = do
     decl <- fromMaybe (error "Z3 var not found") <$> H.lookup (z3_vars z) nm
     withz3c z $ \ctx -> c_Z3_mk_app ctx decl 0 nullPtr

  var_bit z _ nm = do
     decl <- fromMaybe (error "Z3 var not found") <$> H.lookup (z3_vars z) nm
     withz3c z $ \ctx -> c_Z3_mk_app ctx decl 0 nullPtr

  and_bool = baprim c_Z3_mk_and
  or_bool = baprim c_Z3_mk_or
  not_bool = uprim c_Z3_mk_not

  ite_bool z p a b = withz3c z $ \ctx -> c_Z3_mk_ite ctx p a b
  ite_integer z p a b = withz3c z $ \ctx -> c_Z3_mk_ite ctx p a b
  ite_bit z p a b = withz3c z $ \ctx -> c_Z3_mk_ite ctx p a b

  eq_integer = bprim c_Z3_mk_eq
  leq_integer = bprim c_Z3_mk_le
  add_integer = baprim c_Z3_mk_add
  sub_integer = baprim c_Z3_mk_sub

  eq_bit = bprim c_Z3_mk_eq
  leq_bit = bprim c_Z3_mk_bvule
  add_bit = bprim c_Z3_mk_bvadd
  sub_bit = bprim c_Z3_mk_bvsub
  mul_bit = bprim c_Z3_mk_bvmul
  sdiv_bit = bprim c_Z3_mk_bvsdiv
  srem_bit = bprim c_Z3_mk_bvsrem
  smod_bit = bprim c_Z3_mk_bvsmod
  udiv_bit = bprim c_Z3_mk_bvudiv
  urem_bit = bprim c_Z3_mk_bvurem
  or_bit = bprim c_Z3_mk_bvor
  and_bit = bprim c_Z3_mk_bvand
  concat_bit = bprim c_Z3_mk_concat
  shl_bit z _ = bprim c_Z3_mk_bvshl z
  lshr_bit z _ = bprim c_Z3_mk_bvlshr z
  not_bit = uprim c_Z3_mk_bvnot
  sign_extend_bit z fr to a = withz3c z $ \ctx ->
     c_Z3_mk_sign_ext ctx (fromInteger (to - fr)) a
  extract_bit z hi lo x = withz3c z $ \ctx ->
     c_Z3_mk_extract ctx (fromInteger hi) (fromInteger lo) x

{-# SPECIALIZE build :: Z3 -> BoolFF -> IO (Z3Expr, [(FreeID, Type)]) #-}
{-# SPECIALIZE solverFromAST :: IO Z3 -> Solver #-}

z3 :: Solver
z3 = solverFromAST $ do
  r <- c_Z3_load
  case r of
    0 -> return ()
    _ -> error "z3 smten backend: unable to load libz3.so"
  
  cfg <- c_Z3_mk_config
  ctx <- c_Z3_mk_context cfg
  slv <- c_Z3_mk_solver ctx
  c_Z3_solver_inc_ref ctx slv
  vars <- H.new
  c_Z3_del_config cfg
  return $ Z3 ctx slv vars

getBoolValueWithModel :: Z3 -> FreeID -> Z3Model -> IO Bool
getBoolValueWithModel z nm model = do
  v <- var_bool z nm
  alloca $ \pval -> do
    withz3c z $ \ctx -> c_Z3_model_eval ctx model v z3true pval
    val <- peek pval
    bval <- withz3c z $ \ctx -> c_Z3_get_bool_value ctx val
    return (bval == z3ltrue)
  
getIntegerValueWithModel :: Z3 -> FreeID -> Z3Model -> IO Integer
getIntegerValueWithModel z nm model = do
  v <- var_integer z nm
  alloca $ \pval -> do
    withz3c z $ \ctx -> c_Z3_model_eval ctx model v z3true pval
    val <- peek pval
    cstr <- withz3c z $ \ctx -> c_Z3_get_numeral_string ctx val
    str <- peekCString cstr
    return (read str)

getBitVectorValueWithModel :: Z3 -> Integer -> FreeID -> Z3Model -> IO Integer
getBitVectorValueWithModel z w nm model = do
  v <- var_bit z w nm
  alloca $ \pval -> do
    withz3c z $ \ctx -> c_Z3_model_eval ctx model v z3true pval
    val <- peek pval
    cstr <- withz3c z $ \ctx -> c_Z3_get_numeral_string ctx val
    str <- peekCString cstr
    return (read str)

