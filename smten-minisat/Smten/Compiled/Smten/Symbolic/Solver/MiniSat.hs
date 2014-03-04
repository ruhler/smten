
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of the MiniSat backend for smten.
module Smten.Compiled.Smten.Symbolic.Solver.MiniSat (minisat) where

import qualified Data.HashTable.IO as H

import Data.Functor
import Data.Maybe
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.MiniSat.FFI
import Smten.Runtime.Model
import Smten.Runtime.Result
import Smten.Runtime.Bits
import Smten.Runtime.Integers

type VarMap = H.BasicHashTable FreeID MSExpr

data MiniSat = MiniSat {
    s_ctx :: MSSolver,
    s_vars :: VarMap
}

nointegers = error "There is no native support integers in MiniSat"
nobits = error "There is no native support for bit vectors in MiniSat"

minisat :: Solver
minisat = solverFromAST $ do
  ptr <- c_minisat_new
  vars <- H.new
  let base = MiniSat ptr vars
  withints <- addIntegers base
  addBits withints

instance SolverAST MiniSat MSExpr MSExpr MSExpr where
  declare_bool s nm = do 
    v <- c_minisat_var (s_ctx s)
    H.insert (s_vars s) nm v

  declare_integer y nm = nointegers
  declare_bit y w nm = nobits
  
  getBoolValue s nm = do
    r <- H.lookup (s_vars s) nm
    case r of 
       Just v -> do
          r <- c_minisat_getvar (s_ctx s) v
          case r of
            0 -> return False
            1 -> return True
            _ -> error $ "unexpected result from getvar: " ++ show r
       Nothing -> error $ "var " ++ freenm nm ++ " not found"

  getIntegerValue = nointegers
  getBitVectorValue = nobits

  getModel s vars = sequence [BoolA <$> getBoolValue s nm | (nm, BoolT) <- vars]

  check s = do
    r <- c_minisat_check (s_ctx s)
    case r of
       0 -> return Unsat
       1 -> return Sat

  cleanup s = c_minisat_delete (s_ctx s)
  assert s e = c_minisat_assert (s_ctx s) e

  bool s True = c_minisat_true (s_ctx s)
  bool s False = c_minisat_false (s_ctx s)

  integer = nointegers
  bit = nobits

  var_bool s nm = fromJust <$> H.lookup (s_vars s) nm
  var_integer s nm = nointegers
  var_bit s w nm = nobits
    
  and_bool s a b = c_minisat_and (s_ctx s) a b
  or_bool s a b = c_minisat_or (s_ctx s) a b
  not_bool s a = c_minisat_not (s_ctx s) a

  ite_bool s p a b = c_minisat_ite (s_ctx s) p a b
  ite_integer = nointegers
  ite_bit = nobits

  eq_integer = nointegers
  leq_integer = nointegers
  add_integer = nointegers
  sub_integer = nointegers

  eq_bit = nobits
  leq_bit = nobits
  add_bit = nobits
  sub_bit = nobits
  mul_bit = nobits
  or_bit = nobits
  and_bit = nobits
  concat_bit = nobits
  shl_bit = nobits
  lshr_bit = nobits
  not_bit = nobits
  sign_extend_bit = nobits
  extract_bit = nobits

