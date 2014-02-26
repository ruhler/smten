
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of the Yices2 backend for smten.
module Smten.Compiled.Smten.Symbolic.Solver.Yices2 (yices2) where

import Foreign
import Foreign.C.String

import Data.Char
import Data.Functor
import Numeric

import Smten.Runtime.Bit
import Smten.Runtime.Yices2.FFI
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.Model
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Yices2 = Yices2 {
    y2_ctx :: Ptr YContext
}

{-# SPECIALIZE solverFromAST :: IO Yices2 -> Solver #-}

yices2 :: Solver
yices2 = solverFromAST $ do
  r <- c_yices_load
  case r of
    0 -> return ()
    _ -> error "yices2 smten backend: unable to load libyices.so.2.1"
  c_yices_init
  ptr <- c_yices_new_context nullPtr
  return $ Yices2 ptr

withy2 :: Yices2 -> (Ptr YContext -> IO a) -> IO a
withy2 y f = f (y2_ctx y)
        
bvInteger :: [Int32] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)


instance SolverAST Yices2 YTerm where
  declare y BoolT nm = do
    ty <- c_yices_bool_type
    term <- c_yices_new_uninterpreted_term ty
    withCString (freenm nm) $ c_yices_set_term_name term

  declare y IntegerT nm = do
    ty <- c_yices_int_type
    term <- c_yices_new_uninterpreted_term ty
    withCString (freenm nm) $ c_yices_set_term_name term

  declare y (BitT w) nm = do
    ty <- c_yices_bv_type (fromInteger w)
    term <- c_yices_new_uninterpreted_term ty
    withCString (freenm nm) $ c_yices_set_term_name term

  getBoolValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    r <- getboolval yctx nm model
    c_yices_free_model model
    return r

  getIntegerValue y nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    r <- getintegerval yctx nm model
    c_yices_free_model model
    return r

  getBitVectorValue y w nm = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    r <- getbitvectorval yctx w nm model
    c_yices_free_model model
    return r

  getValues y vars = withy2 y $ \yctx -> do
    model <- c_yices_get_model yctx 1
    let getv (nm, BoolT) = BoolA <$> getboolval yctx nm model
        getv (nm, IntegerT) = IntegerA <$> getintegerval yctx nm model
        getv (nm, BitT w) = do
           b <- getbitvectorval yctx w nm model
           return (BitA $ bv_make w b)
    r <- mapM getv vars
    c_yices_free_model model
    return r
    
  check y = {-# SCC "Yices2Check" #-} withy2 y $ \ctx -> do
    st <- c_yices_check_context ctx nullPtr
    return $! fromYSMTStatus st

  cleanup y = do
     withy2 y c_yices_free_context
     c_yices_exit

  assert y e = {-# SCC "Yices2Assert" #-} withy2 y $ \ctx -> c_yices_assert_formula ctx e

  bool _ p = if p then c_yices_true else c_yices_false
  integer _ i = c_yices_int64 (fromInteger i)
  bit _ w v = do
     let w' = fromInteger w
         v' = fromInteger v
         base2 = showIntAtBase 2 (\x -> chr (x + ord '0')) v ""
         binstr = replicate (fromInteger w - length base2) '0' ++ base2
     if w <= 64
        then c_yices_bvconst_uint64 w' v'
        else withCString binstr $ \str -> c_yices_parse_bvbin str

  var _ nm = withCString (freenm nm) c_yices_get_term_by_name

  and_bool _ = c_yices_and2
  or_bool _ = c_yices_or2
  not_bool _ = c_yices_not

  ite_bool _ = c_yices_ite 
  ite_integer _ = c_yices_ite 
  ite_bit _ = c_yices_ite 

  eq_integer _ = c_yices_eq
  leq_integer _ = c_yices_arith_leq_atom
  add_integer _ = c_yices_add
  sub_integer _ = c_yices_sub

  eq_bit _ = c_yices_eq
  leq_bit _ = c_yices_bvle_atom
  add_bit _ = c_yices_bvadd
  sub_bit _ = c_yices_bvsub
  mul_bit _ = c_yices_bvmul
  or_bit _ = c_yices_bvor
  and_bit _ = c_yices_bvand
  concat_bit _ = c_yices_bvconcat
  shl_bit _ _ = c_yices_bvshl
  lshr_bit _ _ = c_yices_bvlshr
  not_bit _ = c_yices_bvnot
  sign_extend_bit _ fr to a = c_yices_sign_extend a (fromInteger $ to - fr)
  extract_bit _ hi lo x = c_yices_bvextract x (fromInteger lo) (fromInteger hi)

getboolval yctx nm model = do
  x <- alloca $ \ptr -> do
          term <- withCString (freenm nm) c_yices_get_term_by_name
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
  case x of
      0 -> return False
      1 -> return True
      _ -> error $ "yices2 get bool value got: " ++ show x

getintegerval yctx nm model = do
  x <- alloca $ \ptr -> do
          term <- withCString (freenm nm) c_yices_get_term_by_name
          ir <- c_yices_get_int64_value model term ptr
          if ir == 0
             then do 
                v <- peek ptr
                return $! v
             else error $ "yices2 get int64 value returned: " ++ show ir
  return $! toInteger x

getbitvectorval ytcx w nm model = do
  bits <- allocaArray (fromInteger w) $ \ptr -> do
      term <- withCString (freenm nm) c_yices_get_term_by_name
      ir <- c_yices_get_bv_value model term ptr
      if ir == 0
          then peekArray (fromInteger w) ptr
          else error $ "yices2 get bit vector value returned: " ++ show ir
  return $! bvInteger bits

