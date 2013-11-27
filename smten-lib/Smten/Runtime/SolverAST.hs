
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.SolverAST (
    SolverAST(..),
    eq_bool, xor_bool, and_bools, or_bools,
    ) where

import Smten.Runtime.FormulaType
import Smten.Runtime.Result

class SolverAST ctx exp | ctx -> exp where
  declare :: ctx -> Type -> String -> IO ()

  getBoolValue :: ctx -> String -> IO Bool
  getIntegerValue :: ctx -> String -> IO Integer
  getBitVectorValue :: ctx -> Integer -> String -> IO Integer

  check :: ctx -> IO Result

  cleanup :: ctx -> IO ()
  cleanup _ = return ()

  assert :: ctx -> exp -> IO ()
  bool :: ctx -> Bool -> IO exp
  integer :: ctx -> Integer -> IO exp
  bit :: ctx -> Integer -> Integer -> IO exp
  var :: ctx -> String -> IO exp

  not_bool :: ctx -> exp -> IO exp

  and_bool :: ctx -> exp -> exp -> IO exp
  and_bool s a b = do
    ff <- bool s False
    ite_bool s a b ff

  or_bool :: ctx -> exp -> exp -> IO exp
  or_bool s a b = do
    tt <- bool s True
    ite_bool s a tt b

  ite_bool :: ctx -> exp -> exp -> exp -> IO exp

  ite_integer :: ctx -> exp -> exp -> exp -> IO exp
  eq_integer :: ctx -> exp -> exp -> IO exp
  leq_integer :: ctx -> exp -> exp -> IO exp
  add_integer :: ctx -> exp -> exp -> IO exp
  sub_integer :: ctx -> exp -> exp -> IO exp

  ite_bit :: ctx -> exp -> exp -> exp -> IO exp
  eq_bit :: ctx -> exp -> exp -> IO exp
  leq_bit :: ctx -> exp -> exp -> IO exp
  add_bit :: ctx -> exp -> exp -> IO exp
  sub_bit :: ctx -> exp -> exp -> IO exp
  mul_bit :: ctx -> exp -> exp -> IO exp
  or_bit :: ctx -> exp -> exp -> IO exp
  and_bit :: ctx -> exp -> exp -> IO exp
  concat_bit :: ctx -> exp -> exp -> IO exp

  -- shl_bit ctx bitwidth a b
  --    Shift left, introducing 0s
  shl_bit :: ctx -> Integer -> exp -> exp -> IO exp

  -- lshr_bit ctx bitwidth a b
  --    Logical shift right (introducing 0s)
  lshr_bit :: ctx -> Integer -> exp -> exp -> IO exp
  not_bit :: ctx -> exp -> IO exp

  -- sign_extend_bit ctx fr to x
  --   fr - the number of bits of x
  --   to - the number of bits after sign extending
  sign_extend_bit :: ctx -> Integer -> Integer -> exp -> IO exp
  
  -- extract ctx hi lo x
  extract_bit :: ctx -> Integer -> Integer -> exp -> IO exp

and_bools :: (SolverAST s e) => s -> [e] -> IO e
and_bools s [] = bool s True
and_bools s [x] = return x
and_bools s (x:xs) = do
  xs' <- and_bools s xs
  and_bool s x xs'

or_bools :: (SolverAST s e) => s -> [e] -> IO e
or_bools s [] = bool s False
or_bools s [x] = return x
or_bools s (x:xs) = do
  xs' <- or_bools s xs
  or_bool s x xs'

eq_bool :: (SolverAST s e) => s -> e -> e -> IO e
eq_bool s a b = do
    notb <- not_bool s b
    ite_bool s a b notb

xor_bool :: (SolverAST s e) => s -> e -> e -> IO e
xor_bool s a b = do
    notb <- not_bool s b
    ite_bool s a notb b

