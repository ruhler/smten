
{-# OPTIONS_GHC -fprof-auto-top #-}

-- | Representation of a finite SMT Formula
module Smten.Runtime.Formula.Finite (
  BoolFF(..), trueFF, falseFF, boolFF, andFF, orFF, notFF, iteFF, varFF,
  IntegerFF(..), integerFF, iaddFF, isubFF, iiteFF, ivarFF, ieqFF, ileqFF,
  BitFF(..), bitFF, bit_varFF, bit_eqFF, bit_leqFF, bit_addFF, bit_subFF,
  bit_mulFF, bit_orFF, bit_andFF, bit_shlFF, bit_lshrFF, bit_concatFF,
  bit_notFF, bit_sign_extendFF, bit_extractFF, bit_iteFF,
  ) where

import Data.Bits

import Smten.Runtime.FreeID
import Smten.Runtime.Bit
import Smten.Runtime.StableNameEq

-- | A boolean finite formula which contains no _|_.
data BoolFF =
   TrueFF
 | FalseFF
 | IteFF BoolFF BoolFF BoolFF
 | AndFF BoolFF BoolFF
 | OrFF BoolFF BoolFF
 | NotFF BoolFF
 | VarFF FreeID
 | IEqFF IntegerFF IntegerFF
 | ILeqFF IntegerFF IntegerFF
 | BitEqFF BitFF BitFF
 | BitLeqFF BitFF BitFF
  deriving (Show)

trueFF :: BoolFF
trueFF = TrueFF

falseFF :: BoolFF
falseFF = FalseFF

boolFF :: Bool -> BoolFF
boolFF True = trueFF
boolFF False = falseFF

varFF :: FreeID -> BoolFF
varFF = VarFF

andFF :: BoolFF -> BoolFF -> BoolFF
andFF TrueFF b = b
andFF FalseFF _ = falseFF
andFF a TrueFF = a
andFF _ FalseFF = falseFF
andFF a b
 | a `stableNameEq` b = a
 | otherwise = AndFF a b
            
notFF :: BoolFF -> BoolFF
notFF TrueFF = FalseFF
notFF FalseFF = TrueFF
notFF (NotFF x) = x
notFF x = NotFF x

orFF :: BoolFF -> BoolFF -> BoolFF
orFF TrueFF _ = trueFF
orFF _ TrueFF = trueFF
orFF FalseFF b = b
orFF a FalseFF = a
orFF a b
 | a `stableNameEq` b = a
 | otherwise = OrFF a b

iteFF :: BoolFF -> BoolFF -> BoolFF -> BoolFF
iteFF TrueFF a _ = a
iteFF FalseFF _ b = b
iteFF (NotFF p) a b = iteFF p b a
iteFF p TrueFF b = orFF p b
iteFF p a FalseFF = andFF p a
iteFF p a TrueFF = orFF (notFF p) a
iteFF p FalseFF b = andFF (notFF p) b
iteFF p a b | a `stableNameEq` b = a
iteFF p a b = IteFF p a b

-- For nicer syntax, we give an instance of Num for BoolFF
-- based on boolean arithmetic.
instance Num BoolFF where
  fromInteger 0 = falseFF
  fromInteger 1 = trueFF
  (+) = orFF
  (*) = andFF
  negate = notFF
  abs = error "BoolFF.abs"
  signum = error "BoolFF.signum"
  
-- | An Integer finite formula which contains no _|_
data IntegerFF =
    IntegerFF Integer
  | IAddFF IntegerFF IntegerFF
  | ISubFF IntegerFF IntegerFF
  | IIteFF BoolFF IntegerFF IntegerFF
  | IVarFF FreeID
  deriving (Show)

integerFF :: Integer -> IntegerFF
integerFF = IntegerFF

ieqFF :: IntegerFF -> IntegerFF -> BoolFF
ieqFF (IntegerFF a) (IntegerFF b) = boolFF (a == b)
ieqFF a b 
 | a `stableNameEq` b = trueFF
 | otherwise = IEqFF a b

ileqFF :: IntegerFF -> IntegerFF -> BoolFF
ileqFF (IntegerFF a) (IntegerFF b) = boolFF (a <= b)
ileqFF a b = ILeqFF a b

iaddFF :: IntegerFF -> IntegerFF -> IntegerFF
iaddFF (IntegerFF a) (IntegerFF b) = IntegerFF (a + b)
iaddFF a b = IAddFF a b

isubFF :: IntegerFF -> IntegerFF -> IntegerFF
isubFF (IntegerFF a) (IntegerFF b) = IntegerFF (a - b)
isubFF a b = ISubFF a b

ivarFF :: FreeID -> IntegerFF
ivarFF = IVarFF

iiteFF :: BoolFF -> IntegerFF -> IntegerFF -> IntegerFF
iiteFF TrueFF a _ = a
iiteFF FalseFF _ b = b
iiteFF p a b = IIteFF p a b

instance Num IntegerFF where
  fromInteger = integerFF
  (+) = iaddFF
  (-) = isubFF
  (*) = error "IntegerFF.*"
  abs = error "IntegerFF.abs"
  signum = error "IntegerFF.signum"


data BitFF =
    BitFF Bit
  | Add_BitFF BitFF BitFF
  | Sub_BitFF BitFF BitFF
  | Mul_BitFF BitFF BitFF
  | Or_BitFF BitFF BitFF
  | And_BitFF BitFF BitFF
  | Shl_BitFF Integer BitFF BitFF   -- ^ Shl bitwidth a b
  | Lshr_BitFF Integer BitFF BitFF  -- ^ Lshr bitwidth a b
  | Concat_BitFF BitFF BitFF   -- ^ Concat a_width a b
  | Not_BitFF BitFF
  | SignExtend_BitFF Integer Integer BitFF    -- ^ SignExtend from_width to_width x
  | Extract_BitFF Integer Integer BitFF -- ^ Extract hi lo x
  | Ite_BitFF BoolFF BitFF BitFF 
  | Var_BitFF Integer FreeID          -- ^ Var width name
     deriving (Show)

bitFF :: Bit -> BitFF
bitFF = BitFF

bit_varFF :: Integer -> FreeID -> BitFF
bit_varFF = Var_BitFF

bit_eqFF :: BitFF -> BitFF -> BoolFF
bit_eqFF (BitFF a) (BitFF b) = boolFF (a == b)
bit_eqFF a b = BitEqFF a b

bit_leqFF ::  BitFF -> BitFF -> BoolFF
bit_leqFF (BitFF a) (BitFF b) = boolFF (a <= b)
bit_leqFF a b = BitLeqFF a b

bit_addFF :: BitFF -> BitFF -> BitFF
bit_addFF (BitFF a) (BitFF b) = BitFF (a + b)
bit_addFF a b = Add_BitFF a b

bit_subFF :: BitFF -> BitFF -> BitFF
bit_subFF (BitFF a) (BitFF b) = BitFF (a - b)
bit_subFF a b = Sub_BitFF a b

bit_mulFF :: BitFF -> BitFF -> BitFF
bit_mulFF (BitFF a) (BitFF b) = BitFF (a * b)
bit_mulFF a b = Mul_BitFF a b

bit_orFF :: BitFF -> BitFF -> BitFF
bit_orFF (BitFF a) (BitFF b) = BitFF (a .|. b)
bit_orFF a b = Or_BitFF a b

bit_andFF :: BitFF -> BitFF -> BitFF
bit_andFF (BitFF a) (BitFF b) = BitFF (a .&. b)
bit_andFF a b = And_BitFF a b

bit_shlFF :: Integer -> BitFF -> BitFF -> BitFF
bit_shlFF _ (BitFF a) (BitFF b) = BitFF (a `bv_shl` b)
bit_shlFF w a b = Shl_BitFF w a b

bit_lshrFF :: Integer -> BitFF -> BitFF -> BitFF
bit_lshrFF _ (BitFF a) (BitFF b) = BitFF (a `bv_lshr` b)
bit_lshrFF w a b = Lshr_BitFF w a b

bit_concatFF :: BitFF -> BitFF -> BitFF
bit_concatFF (BitFF a) (BitFF b) = BitFF (a `bv_concat` b)
bit_concatFF a b = Concat_BitFF a b

bit_notFF :: BitFF -> BitFF
bit_notFF (BitFF a) = BitFF (complement a)
bit_notFF a = Not_BitFF a

bit_sign_extendFF :: Integer -> Integer -> BitFF -> BitFF
bit_sign_extendFF fr to (BitFF a) = BitFF (bv_sign_extend (to-fr) a)
bit_sign_extendFF fr to x = SignExtend_BitFF fr to x

bit_extractFF :: Integer -> Integer -> BitFF -> BitFF
bit_extractFF hi lo (BitFF a) = BitFF (bv_extract hi lo a)
bit_extractFF hi lo x = Extract_BitFF hi lo x

bit_iteFF :: BoolFF -> BitFF -> BitFF -> BitFF
bit_iteFF TrueFF a _ = a
bit_iteFF FalseFF _ b = b
bit_iteFF p a b = Ite_BitFF p a b

