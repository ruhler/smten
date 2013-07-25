
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.SolverAST (SolverAST(..)) where

import Smten.Runtime.Types (Type)
import Smten.Runtime.Result

class SolverAST ctx exp | ctx -> exp where
  declare :: ctx -> Type -> String -> IO ()

  getBoolValue :: ctx -> String -> IO Bool
--  getIntegerValue :: ctx -> String -> IO Integer
--  getBitVectorValue :: ctx -> String -> Integer -> IO Integer

  check :: ctx -> IO Result

  assert :: ctx -> exp -> IO ()
  bool :: ctx -> Bool -> IO exp
--  integer :: ctx -> Integer -> IO exp
--  bit :: ctx -> Integer -> Integer -> IO exp
  var :: ctx -> String -> IO exp

  not_bool :: ctx -> exp -> IO exp
  and_bool :: ctx -> exp -> exp -> IO exp
  ite_bool :: ctx -> exp -> exp -> exp -> IO exp

--  ite_integer :: ctx -> exp -> exp -> exp -> IO exp
--  eq_integer :: ctx -> exp -> exp -> IO exp
--  leq_integer :: ctx -> exp -> exp -> IO exp
--  add_integer :: ctx -> exp -> exp -> IO exp
--  sub_integer :: ctx -> exp -> exp -> IO exp

--  ite_bit :: ctx -> exp -> exp -> exp -> IO exp
--  eq_bit :: ctx -> exp -> exp -> IO exp
--  leq_bit :: ctx -> exp -> exp -> IO exp
--  add_bit :: ctx -> exp -> exp -> IO exp
--  sub_bit :: ctx -> exp -> exp -> IO exp
--  mul_bit :: ctx -> exp -> exp -> IO exp
--  or_bit :: ctx -> exp -> exp -> IO exp
--  and_bit :: ctx -> exp -> exp -> IO exp
--  concat_bit :: ctx -> exp -> exp -> IO exp
--  shl_bit :: ctx -> exp -> exp -> IO exp
--  lshr_bit :: ctx -> exp -> exp -> IO exp
--  not_bit :: ctx -> exp -> IO exp
--
---- sign_extend_bit ctx by x
----   by - the number of bits by which to extend x
--sign_extend_bit :: ctx -> Integer -> exp -> IO exp
--
---- extract ctx hi lo x
--extract_bit :: ctx -> Integer -> Integer -> exp -> IO exp

