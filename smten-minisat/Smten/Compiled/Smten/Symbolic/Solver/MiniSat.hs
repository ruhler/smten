
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Implementation of the MiniSat backend for smten.
module Smten.Compiled.Smten.Symbolic.Solver.MiniSat (minisat) where

import qualified Data.HashTable.IO as H

import Smten.Runtime.Formula.Type
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.MiniSatFFI
import Smten.Runtime.Result
import Smten.Runtime.Bits
import Smten.Runtime.Integers

data Literal = Literal {
  _variable :: MSVar,
  _positive :: Bool
}

-- Positive literal
posL :: MSVar -> Literal
posL v = Literal v True

-- Negative literal
negL :: MSVar -> Literal
negL v = Literal v False

-- Invert a literal
notL :: Literal -> Literal
notL (Literal v p) = Literal v (not p)

-- Note: for now we only support clauses up to length 3.
addclause :: MSSolver -> [Literal] -> IO ()
addclause s [Literal v1 s1] = c_minisat_addclause1 s v1 s1
addclause s [Literal v1 s1,
             Literal v2 s2] = c_minisat_addclause2 s v1 s1 v2 s2
addclause s [Literal v1 s1,
             Literal v2 s2,
             Literal v3 s3] = c_minisat_addclause3 s v1 s1 v2 s2 v3 s3


type VarMap = H.BasicHashTable String MSVar

data MiniSat = MiniSat {
    s_ctx :: MSSolver,
    s_vars :: VarMap
}

nointegers = error "There is no native support integers in MiniSat"
nobits = error "There is no native support for bit vectors in MiniSat"

minisat :: Solver
minisat = solverFromAST $ do
  ptr <- c_minisat_mksolver
  vars <- H.new
  let base = MiniSat ptr vars
  withints <- addIntegers base
  addBits withints

instance SolverAST MiniSat Literal where
  declare s BoolT nm = do 
    v <- c_minisat_mkvar (s_ctx s)
    H.insert (s_vars s) nm v

  declare y IntegerT nm = nointegers
  declare y (BitT w) nm = nobits
  
  getBoolValue s nm = do
    r <- H.lookup (s_vars s) nm
    case r of 
       Just v -> do
          r <- c_minisat_getvar (s_ctx s) v
          case r of
            0 -> return False
            1 -> return True
            _ -> error $ "unexpected result from getvar: " ++ show r
       Nothing -> error $ "var " ++ nm ++ " not found"

  getIntegerValue = nointegers
  getBitVectorValue = nobits

  check s = do
    p <- c_minisat_issat (s_ctx s)
    return $ if p then Sat else Unsat

  cleanup s = c_minisat_delsolver (s_ctx s)

  assert s e = addclause (s_ctx s) [e]

  bool s p = do
    v <- c_minisat_mkvar (s_ctx s)
    addclause (s_ctx s) [Literal v p]
    return (posL v)

  integer = nointegers
  bit = nobits
  var s nm = do
    r <- H.lookup (s_vars s) nm
    case r of
      Just v -> return (posL v)
      Nothing -> error $ "var " ++ nm ++ " not found"
    
  and_bool s a b = do
    x <- c_minisat_mkvar (s_ctx s)
    addclause (s_ctx s) [negL x, a]   -- x ==> a
    addclause (s_ctx s) [negL x, b]   -- x ==> b
    addclause (s_ctx s) [notL a, notL b, posL x]  -- a & b ==> x
    return (posL x)

  or_bool s a b = do
    x <- c_minisat_mkvar (s_ctx s)
    addclause (s_ctx s) [notL a, posL x]  -- a ==> x
    addclause (s_ctx s) [notL b, posL x]  -- b ==> x
    addclause (s_ctx s) [negL x, a, b]    -- x ==> a | b
    return (posL x)

  not_bool s a = return $ notL a

  -- ite p a b      ===>   (p & a) | (~p & b)
  ite_bool s p a b = do
    pa <- and_bool s p a
    npb <- and_bool s (notL p) b
    or_bool s pa npb

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

