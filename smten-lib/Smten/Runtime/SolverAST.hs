

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.SolverAST (
    SolverAST(..),
    eq_bool, xor_bool, and_bools, or_bools,
    ) where

import Data.Functor
import Smten.Runtime.Bit
import Smten.Runtime.Formula.Type
import Smten.Runtime.FreeID
import Smten.Runtime.Model
import Smten.Runtime.Result

class SolverAST ctx boole inte bite
   | ctx -> boole, ctx -> inte, ctx -> bite
 where
  declare_bool :: ctx -> FreeID -> IO ()
  declare_integer :: ctx -> FreeID -> IO ()
  declare_bit :: ctx -> Integer -> FreeID -> IO ()

  getBoolValue :: ctx -> FreeID -> IO Bool
  getIntegerValue :: ctx -> FreeID -> IO Integer
  getBitVectorValue :: ctx -> Integer -> FreeID -> IO Integer

  -- Read a bunch of values all in one go
  -- TODO: probably all the solvers should implement this instead of
  -- using the default.
  getModel :: ctx -> [(FreeID, Type)] -> IO [Any]
  getModel s vars =
    let getValue (f, BoolT) = BoolA <$> getBoolValue s f
        getValue (f, IntegerT) = IntegerA <$> getIntegerValue s f
        getValue (f, BitT w) = do
           b <- getBitVectorValue s w f
           return (BitA $ bv_make w b)
    in mapM getValue vars

  check :: ctx -> IO Result

  cleanup :: ctx -> IO ()
  cleanup _ = return ()

  assert :: ctx -> boole -> IO ()
  bool :: ctx -> Bool -> IO boole
  integer :: ctx -> Integer -> IO inte
  bit :: ctx -> Integer -> Integer -> IO bite

  var_bool :: ctx -> FreeID -> IO boole
  var_integer :: ctx -> FreeID -> IO inte
  var_bit :: ctx -> Integer -> FreeID -> IO bite

  not_bool :: ctx -> boole -> IO boole
  and_bool :: ctx -> boole -> boole -> IO boole
  or_bool :: ctx -> boole -> boole -> IO boole
  ite_bool :: ctx -> boole -> boole -> boole -> IO boole

  ite_integer :: ctx -> boole -> inte -> inte -> IO inte
  eq_integer :: ctx -> inte -> inte -> IO boole
  leq_integer :: ctx -> inte -> inte -> IO boole
  add_integer :: ctx -> inte -> inte -> IO inte
  sub_integer :: ctx -> inte -> inte -> IO inte

  ite_bit :: ctx -> boole -> bite -> bite -> IO bite
  eq_bit :: ctx -> bite -> bite -> IO boole
  leq_bit :: ctx -> bite -> bite -> IO boole
  add_bit :: ctx -> bite -> bite -> IO bite
  sub_bit :: ctx -> bite -> bite -> IO bite
  mul_bit :: ctx -> bite -> bite -> IO bite
  sdiv_bit :: ctx -> bite -> bite -> IO bite
  srem_bit :: ctx -> bite -> bite -> IO bite
  smod_bit :: ctx -> bite -> bite -> IO bite
  udiv_bit :: ctx -> bite -> bite -> IO bite
  urem_bit :: ctx -> bite -> bite -> IO bite
  or_bit :: ctx -> bite -> bite -> IO bite
  and_bit :: ctx -> bite -> bite -> IO bite
  concat_bit :: ctx -> bite -> bite -> IO bite

  -- shl_bit ctx bitwidth a b
  --    Shift left, introducing 0s
  shl_bit :: ctx -> Integer -> bite -> bite -> IO bite

  -- lshr_bit ctx bitwidth a b
  --    Logical shift right (introducing 0s)
  lshr_bit :: ctx -> Integer -> bite -> bite -> IO bite
  not_bit :: ctx -> bite -> IO bite

  -- sign_extend_bit ctx fr to x
  --   fr - the number of bits of x
  --   to - the number of bits after sign extending
  sign_extend_bit :: ctx -> Integer -> Integer -> bite -> IO bite
  
  -- extract ctx hi lo x
  extract_bit :: ctx -> Integer -> Integer -> bite -> IO bite

and_bools :: (SolverAST s b i v) => s -> [b] -> IO b
and_bools s [] = bool s True
and_bools s [x] = return x
and_bools s (x:xs) = do
  xs' <- and_bools s xs
  and_bool s x xs'

or_bools :: (SolverAST s b i v) => s -> [b] -> IO b
or_bools s [] = bool s False
or_bools s [x] = return x
or_bools s (x:xs) = do
  xs' <- or_bools s xs
  or_bool s x xs'

eq_bool :: (SolverAST s b i v) => s -> b -> b -> IO b
eq_bool s a b = do
    notb <- not_bool s b
    ite_bool s a b notb

xor_bool :: (SolverAST s b i v) => s -> b -> b -> IO b
xor_bool s a b = do
    notb <- not_bool s b
    ite_bool s a notb b

