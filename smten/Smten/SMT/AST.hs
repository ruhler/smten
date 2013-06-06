
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Class for SMT solver's abstract syntax tree.
module Smten.SMT.AST (AST(..)) where

class AST ctx exp | ctx -> exp where
  assert :: ctx -> exp -> IO ()
  bool :: ctx -> Bool -> IO exp
  integer :: ctx -> Integer -> IO exp
  bit :: ctx -> Integer -> Integer -> IO exp
  var :: ctx -> String -> IO exp
  ite :: ctx -> exp -> exp -> exp -> IO exp

  eq_integer :: ctx -> exp -> exp -> IO exp
  leq_integer :: ctx -> exp -> exp -> IO exp
  add_integer :: ctx -> exp -> exp -> IO exp
  sub_integer :: ctx -> exp -> exp -> IO exp

  eq_bit :: ctx -> exp -> exp -> IO exp
  leq_bit :: ctx -> exp -> exp -> IO exp
  add_bit :: ctx -> exp -> exp -> IO exp
  sub_bit :: ctx -> exp -> exp -> IO exp
  mul_bit :: ctx -> exp -> exp -> IO exp

