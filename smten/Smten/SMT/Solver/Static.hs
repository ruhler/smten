
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.SMT.Solver.Static (Result(..), Solver(..)) where

data Result
    = Satisfiable
    | Unsatisfiable
    deriving (Eq, Show)

class Solver ctx exp | ctx -> exp where
  declare_bool :: ctx -> String -> IO ()
  declare_integer :: ctx -> String -> IO ()
  declare_bit :: ctx -> String -> Integer -> IO ()

  getBoolValue :: ctx -> String -> IO Bool
  getIntegerValue :: ctx -> String -> IO Integer
  getBitVectorValue :: ctx -> String -> Integer -> IO Integer

  check :: ctx -> IO Result

  assert :: ctx -> exp -> IO ()
  bool :: ctx -> Bool -> IO exp
  integer :: ctx -> Integer -> IO exp
  bit :: ctx -> Integer -> Integer -> IO exp
  var :: ctx -> String -> IO exp

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

