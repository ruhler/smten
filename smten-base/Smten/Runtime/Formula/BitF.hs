
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds #-}

module Smten.Runtime.Formula.BitF (
    BitF(..), eq_Bit, leq_Bit, add_Bit, sub_Bit, mul_Bit,
    or_Bit, and_Bit, shl_Bit, lshr_Bit, not_Bit, concat_Bit,
    sign_extend_Bit, extract_Bit,
  ) where

import Data.Bits
import GHC.TypeLits

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.BoolF

data BitF (n :: Nat) where
  BitF :: Bit -> BitF n
  Bit_Add :: BitF n -> BitF n -> BitF n
  Bit_Sub :: BitF n -> BitF n -> BitF n
  Bit_Mul :: BitF n -> BitF n -> BitF n
  Bit_Or :: BitF n -> BitF n -> BitF n
  Bit_And :: BitF n -> BitF n -> BitF n
  Bit_Shl :: BitF n -> BitF n -> BitF n
  Bit_Lshr :: BitF n -> BitF n -> BitF n

  -- Bit_Concat a_width a b 
  Bit_Concat :: Integer -> BitF a -> BitF b -> BitF n
  Bit_Not :: BitF n -> BitF n

  -- Bit_SignExtend by x
  Bit_SignExtend :: Integer -> BitF m -> BitF n

  -- Bit_Extract x_width hi lo x
  Bit_Extract :: Integer -> Integer -> Integer -> BitF m -> BitF n
  Bit_Ite :: BoolF -> BitF n -> BitF n -> BitF n
  Bit_Var :: Integer -> FreeID -> BitF n

instance Show (BitF n) where
  show (BitF x) = show x
  show (Bit_Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Bit_Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Bit_Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Bit_Or a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (Bit_And a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (Bit_Shl {}) = "?Bit_Shl?"
  show (Bit_Lshr {}) = "?Bit_Lshr?"
  show (Bit_Concat {}) = "?Bit_Concat?"
  show (Bit_Not a) = "~ " ++ show a
  show (Bit_SignExtend {}) = "?Bit_SignExtend?"
  show (Bit_Extract _ hi lo x) = show x ++ "[" ++ show hi ++ ":" ++ show lo ++ "]"
  show (Bit_Ite p a b) = "(" ++ show p ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
  show (Bit_Var _ x) = freenm x
    

eq_Bit :: Integer -> BitF n -> BitF n -> BoolF
eq_Bit _ (BitF a) (BitF b) = boolF (a == b)
eq_Bit w a b = error "TODO: eq_Bit with symbolic args"

leq_Bit :: Integer -> BitF n -> BitF n -> BoolF
leq_Bit _ (BitF a) (BitF b) = boolF (a <= b)
leq_Bit w a b = error "TODO: leq_Bit with symbolic args"

add_Bit :: BitF n -> BitF n -> BitF n
add_Bit (BitF a) (BitF b) = BitF (a + b)
add_Bit a b = Bit_Add a b

sub_Bit :: BitF n -> BitF n -> BitF n
sub_Bit (BitF a) (BitF b) = BitF (a - b)
sub_Bit a b = Bit_Sub a b

mul_Bit :: BitF n -> BitF n -> BitF n
mul_Bit (BitF a) (BitF b) = BitF (a * b)
mul_Bit a b = Bit_Mul a b

or_Bit :: BitF n -> BitF n -> BitF n
or_Bit (BitF a) (BitF b) = BitF (a .|. b)
or_Bit a b = Bit_Or a b

and_Bit :: BitF n -> BitF n -> BitF n
and_Bit (BitF a) (BitF b) = BitF (a .&. b)
and_Bit a b = Bit_And a b

shl_Bit :: BitF n -> BitF n -> BitF n
shl_Bit (BitF a) (BitF b) = BitF (a `bv_shl` b)
shl_Bit a b = Bit_Shl a b

lshr_Bit :: BitF n -> BitF n -> BitF n
lshr_Bit (BitF a) (BitF b) = BitF (a `bv_lshr` b)
lshr_Bit a b = Bit_Lshr a b

concat_Bit :: Integer -> BitF a -> BitF b -> BitF n
concat_Bit _ (BitF a) (BitF b) = BitF (a `bv_concat` b)
concat_Bit w a b = Bit_Concat w a b

not_Bit :: BitF n -> BitF n
not_Bit (BitF a) = BitF (complement a)
not_Bit a = Bit_Not a

sign_extend_Bit :: Integer -> BitF m -> BitF n
sign_extend_Bit by (BitF a) = BitF (bv_sign_extend by a)
sign_extend_Bit by x = Bit_SignExtend by x

extract_Bit :: Integer -> Integer -> Integer -> BitF m -> BitF n
extract_Bit _ hi lo (BitF a) = BitF (bv_extract hi lo a)
extract_Bit wx hi lo x = Bit_Extract wx hi lo x

