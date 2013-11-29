
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds #-}

module Smten.Runtime.Formula.BitF (
    BitF(..), bit_eqF, bit_leqF, bit_addF, bit_subF, bit_mulF,
    bit_orF, bit_andF, bit_shlF, bit_lshrF, bit_notF, bit_concatF,
    bit_sign_extendF, bit_extractF,
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
    

bit_eqF :: Integer -> BitF n -> BitF n -> BoolF
bit_eqF _ (BitF a) (BitF b) = boolF (a == b)
bit_eqF w a b = error "TODO: bit_eqF with symbolic args"

bit_leqF :: Integer -> BitF n -> BitF n -> BoolF
bit_leqF _ (BitF a) (BitF b) = boolF (a <= b)
bit_leqF w a b = error "TODO: bit_leqF with symbolic args"

bit_addF :: BitF n -> BitF n -> BitF n
bit_addF (BitF a) (BitF b) = BitF (a + b)
bit_addF a b = Bit_Add a b

bit_subF :: BitF n -> BitF n -> BitF n
bit_subF (BitF a) (BitF b) = BitF (a - b)
bit_subF a b = Bit_Sub a b

bit_mulF :: BitF n -> BitF n -> BitF n
bit_mulF (BitF a) (BitF b) = BitF (a * b)
bit_mulF a b = Bit_Mul a b

bit_orF :: BitF n -> BitF n -> BitF n
bit_orF (BitF a) (BitF b) = BitF (a .|. b)
bit_orF a b = Bit_Or a b

bit_andF :: BitF n -> BitF n -> BitF n
bit_andF (BitF a) (BitF b) = BitF (a .&. b)
bit_andF a b = Bit_And a b

bit_shlF :: BitF n -> BitF n -> BitF n
bit_shlF (BitF a) (BitF b) = BitF (a `bv_shl` b)
bit_shlF a b = Bit_Shl a b

bit_lshrF :: BitF n -> BitF n -> BitF n
bit_lshrF (BitF a) (BitF b) = BitF (a `bv_lshr` b)
bit_lshrF a b = Bit_Lshr a b

bit_concatF :: Integer -> BitF a -> BitF b -> BitF n
bit_concatF _ (BitF a) (BitF b) = BitF (a `bv_concat` b)
bit_concatF w a b = Bit_Concat w a b

bit_notF :: BitF n -> BitF n
bit_notF (BitF a) = BitF (complement a)
bit_notF a = Bit_Not a

bit_sign_extendF :: Integer -> BitF m -> BitF n
bit_sign_extendF by (BitF a) = BitF (bv_sign_extend by a)
bit_sign_extendF by x = Bit_SignExtend by x

bit_extractF :: Integer -> Integer -> Integer -> BitF m -> BitF n
bit_extractF _ hi lo (BitF a) = BitF (bv_extract hi lo a)
bit_extractF wx hi lo x = Bit_Extract wx hi lo x

