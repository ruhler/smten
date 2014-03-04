
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Backend for the Yices1 Solver
module Smten.Compiled.Smten.Symbolic.Solver.Yices1 (yices1) where

import Data.Functor
import qualified Data.HashTable.IO as H

import Foreign hiding (bit)
import Foreign.C.String
import Foreign.C.Types

import Smten.Runtime.Bit
import Smten.Runtime.Build
import Smten.Runtime.Yices1.FFI
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.Model
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

type VarMap = H.BasicHashTable FreeID YDecl

data Yices1 = Yices1 {
    y1_ctx :: YContext,
    y1_vars :: VarMap
}

withy1 :: Yices1 -> (YContext -> IO a) -> IO a
withy1 y f = f (y1_ctx y)

getdecl :: Yices1 -> FreeID -> IO YDecl
getdecl y nm = do
    r <- H.lookup (y1_vars y) nm
    case r of
        Just v -> return v
        Nothing -> error $"Yices1: unknown var: " ++ freenm nm

bvInteger :: [CInt] -> Integer
bvInteger [] = 0
bvInteger (x:xs) = bvInteger xs * 2 + (fromIntegral x)

bvBits :: Integer -> Integer -> [CInt]
bvBits w v = 
  let bitx :: Int -> CInt
      bitx p = if testBit v p then 1 else 0
  in map bitx [0..(fromInteger w - 1)]

uprim :: (YContext -> YExpr -> IO YExpr) ->
         Yices1 -> YExpr -> IO YExpr
uprim f y a = withy1 y $ \ctx -> f ctx a

bprim :: (YContext -> YExpr -> YExpr -> IO YExpr) ->
         Yices1 -> YExpr -> YExpr -> IO YExpr
bprim f y a b = withy1 y $ \ctx -> f ctx a b

baprim :: (YContext -> Ptr YExpr -> CUInt -> IO YExpr) ->
          Yices1 -> YExpr -> YExpr -> IO YExpr
baprim f y a b = withy1 y $ \ctx -> withArray [a, b] $ \arr -> f ctx arr 2

-- Yices1 does not expose a generic left shift function. Only left shift by
-- constants. This makes a generic version based on that.
shl_bit_from :: Yices1 -> Integer -> Integer -> YExpr -> YExpr -> IO YExpr
shl_bit_from y fr w a b 
  | fr == w = bit y w 0
  | otherwise = do
      n <- bit y w fr
      eq <- eq_bit y b n
      sh <- withy1 y (\ctx -> c_yices_mk_bv_shift_left0 ctx a (fromInteger fr))
      rest <- shl_bit_from y (fr+1) w a b
      ite_bit y eq sh rest

-- Yices1 does not expose a generic right shift function. Only right shift by
-- constants. This makes a generic version based on that.
lshr_bit_from :: Yices1 -> Integer -> Integer -> YExpr -> YExpr -> IO YExpr
lshr_bit_from y fr w a b 
  | fr == w = bit y w 0
  | otherwise = do
      n <- bit y w fr
      eq <- eq_bit y b n
      sh <- withy1 y (\ctx -> c_yices_mk_bv_shift_right0 ctx a (fromInteger fr))
      rest <- lshr_bit_from y (fr+1) w a b
      ite_bit y eq sh rest

instance SolverAST Yices1 YExpr YExpr YExpr where
  declare_bool y nm = do
      y1ty <- withCString "bool" $ \tynm ->
                  withy1 y $ \ctx -> c_yices_mk_type ctx tynm
      decl <- withCString (freenm nm) $ \str ->
                withy1 y $ \yctx ->
                  c_yices_mk_var_decl yctx str y1ty
      H.insert (y1_vars y) nm decl

  declare_integer y nm = do
      y1ty <- withCString "int" $ \tynm ->
                 withy1 y $ \ctx -> c_yices_mk_type ctx tynm
      decl <- withCString (freenm nm) $ \str ->
                withy1 y $ \yctx ->
                  c_yices_mk_var_decl yctx str y1ty
      H.insert (y1_vars y) nm decl

  declare_bit y w nm = do
      y1ty <- withy1 y $ \ctx -> c_yices_mk_bitvector_type ctx (fromInteger w)
      decl <- withCString (freenm nm) $ \str ->
                withy1 y $ \yctx ->
                  c_yices_mk_var_decl yctx str y1ty
      H.insert (y1_vars y) nm decl

  getBoolValue y nm = do
    model <- withy1 y c_yices_get_model 
    getBoolValueWithModel y nm model

  getIntegerValue y nm = do
    model <- withy1 y c_yices_get_model 
    getIntegerValueWithModel y nm model

  getBitVectorValue y w nm = do
    model <- withy1 y c_yices_get_model 
    getBitVectorValueWithModel y w nm model

  getModel y vars = do
    model <- withy1 y c_yices_get_model
    let getv (nm, BoolT) = BoolA <$> getBoolValueWithModel y nm model
        getv (nm, IntegerT) = IntegerA <$> getIntegerValueWithModel y nm model
        getv (nm, BitT w) = do
           b <- getBitVectorValueWithModel y w nm model
           return (BitA $ bv_make w b)
    r <- mapM getv vars
    return r

  check y = do
    res <- withy1 y c_yices_check
    return $ toResult res

  cleanup y = withy1 y c_yices_del_context

  assert y p = withy1 y $ \ctx -> c_yices_assert ctx p
  bool y True = withy1 y c_yices_mk_true
  bool y False = withy1 y c_yices_mk_false
  integer y i = withy1 y $ \ctx -> c_yices_mk_num ctx (fromInteger i)
  bit y w v = withy1 y $ \ctx -> do
        let w' = fromInteger w
            v' = fromInteger v
        if w <= 32
            then c_yices_mk_bv_constant ctx w' v'
            else do
              let bits = bvBits w v
              r <- withArray bits $ c_yices_mk_bv_constant_from_array ctx w'
              return r

  var_bool y nm = withy1 y $ \ctx -> do
     decl <- getdecl y nm
     c_yices_mk_var_from_decl ctx decl

  var_integer y nm = withy1 y $ \ctx -> do
     decl <- getdecl y nm
     c_yices_mk_var_from_decl ctx decl

  var_bit y w nm = withy1 y $ \ctx -> do
     decl <- getdecl y nm
     c_yices_mk_var_from_decl ctx decl


  and_bool = baprim c_yices_mk_and
  or_bool = baprim c_yices_mk_or
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
  shl_bit y = shl_bit_from y 0
  lshr_bit y = lshr_bit_from y 0
  not_bit = uprim c_yices_mk_bv_not
  sign_extend_bit y fr to a = withy1 y $ \ctx ->
     c_yices_mk_bv_sign_extend ctx a (fromInteger (to - fr))
  extract_bit y hi lo x = withy1 y $ \ctx ->
     c_yices_mk_bv_extract ctx (fromInteger hi) (fromInteger lo) x

{-# SPECIALIZE build :: Yices1 -> BoolFF -> IO (YExpr, [(FreeID, Type)]) #-}
{-# SPECIALIZE solverFromAST :: IO Yices1 -> Solver #-}

yices1 :: Solver
yices1 = solverFromAST $ do
  ptr <- c_yices_mk_context
  vars <- H.new
  return $ Yices1 ptr vars

getBoolValueWithModel :: Yices1 -> FreeID -> YModel -> IO Bool
getBoolValueWithModel y nm model = do
  decl <- getdecl y nm
  br <- c_yices_get_value model decl
  case br of
    _ | br == yTrue -> return True
    _ | br == yFalse -> return False
    _ | br == yUndef -> return False

getIntegerValueWithModel :: Yices1 -> FreeID -> YModel -> IO Integer
getIntegerValueWithModel y nm model = do
  decl <- getdecl y nm
  alloca $ \ptr -> do
    ir <- c_yices_get_int_value model decl ptr
    if ir == 1
        then toInteger <$> peek ptr
        else return 0

getBitVectorValueWithModel :: Yices1 -> Integer -> FreeID -> YModel -> IO Integer
getBitVectorValueWithModel y w nm model = do
  decl <- getdecl y nm
  allocaArray (fromInteger w) $ \ptr -> do
      ir <- c_yices_get_bitvector_value model decl (fromInteger w) ptr
      if ir == 1
          then bvInteger <$> peekArray (fromInteger w) ptr
          else return 0
