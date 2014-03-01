
{-# OPTIONS_GHC -auto-all #-}

-- | Representation of a finite SMT Formula
module Smten.Runtime.Formula.Finite (
  BoolFF(..), trueFF, falseFF, boolFF, andFF, orFF, notFF, iteFF, varFF,

  IntegerFF(..), integerFF, ite_IntegerFF, var_IntegerFF,
  eq_IntegerFF, leq_IntegerFF, add_IntegerFF, sub_IntegerFF,

  BitFF(..), bitFF, var_BitFF, eq_BitFF, leq_BitFF, add_BitFF, sub_BitFF,
  mul_BitFF, bit_orFF, bit_andFF, bit_shlFF, bit_lshrFF, bit_concatFF,
  bit_notFF, bit_sign_extendFF, bit_extractFF, ite_BitFF,
  ) where

import Data.Bits

import Smten.Runtime.BuildCache
import Smten.Runtime.FreeID
import Smten.Runtime.Bit
import Smten.Runtime.StableNameEq

-- | A boolean finite formula which contains no _|_.
data BoolFF =
   TrueFF
 | FalseFF
 | IteFF BoolFF BoolFF BoolFF BuildCache
 | AndFF BoolFF BoolFF BuildCache
 | OrFF BoolFF BoolFF BuildCache
 | NotFF BoolFF BuildCache
 | VarFF FreeID BuildCache
 | Eq_IntegerFF IntegerFF IntegerFF BuildCache
 | Leq_IntegerFF IntegerFF IntegerFF BuildCache
 | Eq_BitFF BitFF BitFF BuildCache
 | Leq_BitFF BitFF BitFF BuildCache
 | Unreachable_BoolFF
    deriving (Show)

trueFF :: BoolFF
trueFF = TrueFF

falseFF :: BoolFF
falseFF = FalseFF

boolFF :: Bool -> BoolFF
boolFF True = trueFF
boolFF False = falseFF

varFF :: FreeID -> BoolFF
varFF x = new (VarFF x)

andFF :: BoolFF -> BoolFF -> BoolFF
andFF TrueFF b = b
andFF FalseFF _ = falseFF
andFF a TrueFF = a
andFF _ FalseFF = falseFF
andFF Unreachable_BoolFF Unreachable_BoolFF = Unreachable_BoolFF
andFF Unreachable_BoolFF _ = falseFF
andFF _ Unreachable_BoolFF = falseFF
andFF a b
 | a `stableNameEq` b = a
 | otherwise = new (AndFF a b)
            
notFF :: BoolFF -> BoolFF
notFF TrueFF = FalseFF
notFF FalseFF = TrueFF
notFF (NotFF x _) = x
notFF Unreachable_BoolFF = Unreachable_BoolFF
notFF x = new (NotFF x)

orFF :: BoolFF -> BoolFF -> BoolFF
orFF TrueFF _ = trueFF
orFF _ TrueFF = trueFF
orFF FalseFF b = b
orFF a FalseFF = a
orFF Unreachable_BoolFF Unreachable_BoolFF = Unreachable_BoolFF
orFF Unreachable_BoolFF _ = trueFF
orFF _ Unreachable_BoolFF = trueFF
orFF a b
 | a `stableNameEq` b = a
 | otherwise = new (OrFF a b)

iteFF :: BoolFF -> BoolFF -> BoolFF -> BoolFF
iteFF TrueFF a _ = a
iteFF FalseFF _ b = b
iteFF (NotFF p _) a b = iteFF p b a
iteFF Unreachable_BoolFF _ _ = Unreachable_BoolFF
iteFF p TrueFF b = orFF p b
iteFF p a FalseFF = andFF p a
iteFF p a TrueFF = orFF (notFF p) a
iteFF p FalseFF b = andFF (notFF p) b
iteFF _ Unreachable_BoolFF b = b
iteFF _ a Unreachable_BoolFF = a
iteFF p a b | a `stableNameEq` b = a
iteFF p a b = new (IteFF p a b)

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
  | Add_IntegerFF IntegerFF IntegerFF BuildCache
  | Sub_IntegerFF IntegerFF IntegerFF BuildCache
  | Ite_IntegerFF BoolFF IntegerFF IntegerFF BuildCache
  | Var_IntegerFF FreeID BuildCache
  | Unreachable_IntegerFF
  deriving (Show)

integerFF :: Integer -> IntegerFF
integerFF = IntegerFF

eq_IntegerFF :: IntegerFF -> IntegerFF -> BoolFF
eq_IntegerFF (IntegerFF a) (IntegerFF b) = boolFF (a == b)
eq_IntegerFF Unreachable_IntegerFF _ = Unreachable_BoolFF
eq_IntegerFF _ Unreachable_IntegerFF = Unreachable_BoolFF
eq_IntegerFF a b 
 | a `stableNameEq` b = trueFF
 | otherwise = new (Eq_IntegerFF a b)

leq_IntegerFF :: IntegerFF -> IntegerFF -> BoolFF
leq_IntegerFF (IntegerFF a) (IntegerFF b) = boolFF (a <= b)
leq_IntegerFF Unreachable_IntegerFF _ = Unreachable_BoolFF
leq_IntegerFF _ Unreachable_IntegerFF = Unreachable_BoolFF
leq_IntegerFF a b = new (Leq_IntegerFF a b)

add_IntegerFF :: IntegerFF -> IntegerFF -> IntegerFF
add_IntegerFF (IntegerFF a) (IntegerFF b) = IntegerFF (a + b)
add_IntegerFF Unreachable_IntegerFF _ = Unreachable_IntegerFF
add_IntegerFF _ Unreachable_IntegerFF = Unreachable_IntegerFF
add_IntegerFF a b = new (Add_IntegerFF a b)

sub_IntegerFF :: IntegerFF -> IntegerFF -> IntegerFF
sub_IntegerFF (IntegerFF a) (IntegerFF b) = IntegerFF (a - b)
sub_IntegerFF Unreachable_IntegerFF _ = Unreachable_IntegerFF
sub_IntegerFF _ Unreachable_IntegerFF = Unreachable_IntegerFF
sub_IntegerFF a b = new (Sub_IntegerFF a b)

var_IntegerFF :: FreeID -> IntegerFF
var_IntegerFF x = new (Var_IntegerFF x)

ite_IntegerFF :: BoolFF -> IntegerFF -> IntegerFF -> IntegerFF
ite_IntegerFF TrueFF a _ = a
ite_IntegerFF FalseFF _ b = b
ite_IntegerFF Unreachable_BoolFF _ _ = Unreachable_IntegerFF
ite_IntegerFF p v@(IntegerFF a) (IntegerFF b) | a == b = v
ite_IntegerFF _ Unreachable_IntegerFF b = b
ite_IntegerFF _ a Unreachable_IntegerFF = a
ite_IntegerFF p a b = new (Ite_IntegerFF p a b)

instance Num IntegerFF where
  fromInteger = integerFF
  (+) = add_IntegerFF
  (-) = sub_IntegerFF
  (*) = error "IntegerFF.*"
  abs = error "IntegerFF.abs"
  signum = error "IntegerFF.signum"


data BitFF =
    BitFF Bit
  | Add_BitFF BitFF BitFF BuildCache
  | Sub_BitFF BitFF BitFF BuildCache
  | Mul_BitFF BitFF BitFF BuildCache
  | Or_BitFF BitFF BitFF BuildCache
  | And_BitFF BitFF BitFF BuildCache
  | Shl_BitFF Integer BitFF BitFF BuildCache  -- ^ Shl bitwidth a b
  | Lshr_BitFF Integer BitFF BitFF BuildCache -- ^ Lshr bitwidth a b
  | Concat_BitFF BitFF BitFF BuildCache  -- ^ Concat a_width a b
  | Not_BitFF BitFF BuildCache
  | SignExtend_BitFF Integer Integer BitFF BuildCache   -- ^ SignExtend from_width to_width x
  | Extract_BitFF Integer Integer BitFF BuildCache -- ^ Extract hi lo x
  | Ite_BitFF BoolFF BitFF BitFF BuildCache 
  | Var_BitFF Integer FreeID BuildCache         -- ^ Var width name
  | Unreachable_BitFF
     deriving (Show)

bitFF :: Bit -> BitFF
bitFF = BitFF

var_BitFF :: Integer -> FreeID -> BitFF
var_BitFF x f = new (Var_BitFF x f)
 
eq_BitFF :: BitFF -> BitFF -> BoolFF
eq_BitFF (BitFF a) (BitFF b) = boolFF (a == b)
eq_BitFF Unreachable_BitFF _ = Unreachable_BoolFF
eq_BitFF _ Unreachable_BitFF = Unreachable_BoolFF
eq_BitFF a b = new (Eq_BitFF a b)

leq_BitFF ::  BitFF -> BitFF -> BoolFF
leq_BitFF (BitFF a) (BitFF b) = boolFF (a <= b)
leq_BitFF Unreachable_BitFF _ = Unreachable_BoolFF
leq_BitFF _ Unreachable_BitFF = Unreachable_BoolFF
leq_BitFF a b = new (Leq_BitFF a b)

add_BitFF :: BitFF -> BitFF -> BitFF
add_BitFF (BitFF a) (BitFF b) = BitFF (a + b)
add_BitFF Unreachable_BitFF _ = Unreachable_BitFF
add_BitFF _ Unreachable_BitFF = Unreachable_BitFF
add_BitFF a b = new (Add_BitFF a b)

sub_BitFF :: BitFF -> BitFF -> BitFF
sub_BitFF (BitFF a) (BitFF b) = BitFF (a - b)
sub_BitFF Unreachable_BitFF _ = Unreachable_BitFF
sub_BitFF _ Unreachable_BitFF = Unreachable_BitFF
sub_BitFF a b = new (Sub_BitFF a b)

mul_BitFF :: BitFF -> BitFF -> BitFF
mul_BitFF (BitFF a) (BitFF b) = BitFF (a * b)
mul_BitFF Unreachable_BitFF _ = Unreachable_BitFF
mul_BitFF _ Unreachable_BitFF = Unreachable_BitFF
mul_BitFF a b = new (Mul_BitFF a b)

bit_orFF :: BitFF -> BitFF -> BitFF
bit_orFF (BitFF a) (BitFF b) = BitFF (a .|. b)
bit_orFF Unreachable_BitFF _ = Unreachable_BitFF
bit_orFF _ Unreachable_BitFF = Unreachable_BitFF
bit_orFF a b = new (Or_BitFF a b)

bit_andFF :: BitFF -> BitFF -> BitFF
bit_andFF (BitFF a) (BitFF b) = BitFF (a .&. b)
bit_andFF Unreachable_BitFF _ = Unreachable_BitFF
bit_andFF _ Unreachable_BitFF = Unreachable_BitFF
bit_andFF a b = new (And_BitFF a b)

bit_shlFF :: Integer -> BitFF -> BitFF -> BitFF
bit_shlFF _ (BitFF a) (BitFF b) = BitFF (a `bv_shl` b)
bit_shlFF _ Unreachable_BitFF _ = Unreachable_BitFF
bit_shlFF _ _ Unreachable_BitFF = Unreachable_BitFF
bit_shlFF w a b = new (Shl_BitFF w a b)

bit_lshrFF :: Integer -> BitFF -> BitFF -> BitFF
bit_lshrFF _ (BitFF a) (BitFF b) = BitFF (a `bv_lshr` b)
bit_lshrFF _ Unreachable_BitFF _ = Unreachable_BitFF
bit_lshrFF _ _ Unreachable_BitFF = Unreachable_BitFF
bit_lshrFF w a b = new (Lshr_BitFF w a b)

bit_concatFF :: BitFF -> BitFF -> BitFF
bit_concatFF (BitFF a) (BitFF b) = BitFF (a `bv_concat` b)
bit_concatFF Unreachable_BitFF _ = Unreachable_BitFF
bit_concatFF _ Unreachable_BitFF = Unreachable_BitFF
bit_concatFF a b = new (Concat_BitFF a b)

bit_notFF :: BitFF -> BitFF
bit_notFF (BitFF a) = BitFF (complement a)
bit_notFF Unreachable_BitFF = Unreachable_BitFF
bit_notFF a = new (Not_BitFF a)

bit_sign_extendFF :: Integer -> Integer -> BitFF -> BitFF
bit_sign_extendFF fr to (BitFF a) = BitFF (bv_sign_extend (to-fr) a)
bit_sign_extendFF _ _ Unreachable_BitFF = Unreachable_BitFF
bit_sign_extendFF fr to x = new (SignExtend_BitFF fr to x)

bit_extractFF :: Integer -> Integer -> BitFF -> BitFF
bit_extractFF hi lo (BitFF a) = BitFF (bv_extract hi lo a)
bit_extractFF _ _ Unreachable_BitFF = Unreachable_BitFF
bit_extractFF hi lo x = new (Extract_BitFF hi lo x)

ite_BitFF :: BoolFF -> BitFF -> BitFF -> BitFF
ite_BitFF TrueFF a _ = a
ite_BitFF FalseFF _ b = b
ite_BitFF Unreachable_BoolFF _ _ = Unreachable_BitFF
ite_BitFF p v@(BitFF a) (BitFF b) | a == b = v
ite_BitFF _ Unreachable_BitFF b = b
ite_BitFF _ a Unreachable_BitFF = a
ite_BitFF p a b = new (Ite_BitFF p a b)

