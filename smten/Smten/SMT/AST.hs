
-- Class for SMT solver's abstract syntax tree.
module Smten.SMT.AST (AST(..)) where

import Data.Dynamic

class AST ctx where
  assert :: ctx -> Dynamic -> IO ()
  bool :: ctx -> Bool -> IO Dynamic
  integer :: ctx -> Integer -> IO Dynamic
  bit :: ctx -> Integer -> Integer -> IO Dynamic
  var :: ctx -> String -> IO Dynamic

  ite_bool :: ctx -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic

  ite_integer :: ctx -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic
  eq_integer :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  leq_integer :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  add_integer :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  sub_integer :: ctx -> Dynamic -> Dynamic -> IO Dynamic

  ite_bit :: ctx -> Dynamic -> Dynamic -> Dynamic -> IO Dynamic
  eq_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  leq_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  add_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  sub_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  mul_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic
  or_bit :: ctx -> Dynamic -> Dynamic -> IO Dynamic

