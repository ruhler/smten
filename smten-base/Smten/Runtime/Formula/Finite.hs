
{-# OPTIONS_GHC -fprof-auto-top #-}

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
 | BoolFF_Unreachable
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
andFF BoolFF_Unreachable BoolFF_Unreachable = BoolFF_Unreachable
andFF BoolFF_Unreachable _ = falseFF
andFF _ BoolFF_Unreachable = falseFF
andFF a b
 | a `stableNameEq` b = a
 | otherwise = AndFF a b
            
notFF :: BoolFF -> BoolFF
notFF TrueFF = FalseFF
notFF FalseFF = TrueFF
notFF (NotFF x) = x
notFF BoolFF_Unreachable = BoolFF_Unreachable
notFF x = NotFF x

orFF :: BoolFF -> BoolFF -> BoolFF
orFF TrueFF _ = trueFF
orFF _ TrueFF = trueFF
orFF FalseFF b = b
orFF a FalseFF = a
orFF BoolFF_Unreachable BoolFF_Unreachable = BoolFF_Unreachable
orFF BoolFF_Unreachable _ = trueFF
orFF _ BoolFF_Unreachable = trueFF
orFF a b
 | a `stableNameEq` b = a
 | otherwise = OrFF a b

iteFF :: BoolFF -> BoolFF -> BoolFF -> BoolFF
iteFF TrueFF a _ = a
iteFF FalseFF _ b = b
iteFF (NotFF p) a b = iteFF p b a
iteFF BoolFF_Unreachable _ _ = BoolFF_Unreachable
iteFF p TrueFF b = orFF p b
iteFF p a FalseFF = andFF p a
iteFF p a TrueFF = orFF (notFF p) a
iteFF p FalseFF b = andFF (notFF p) b
iteFF _ BoolFF_Unreachable b = b
iteFF _ a BoolFF_Unreachable = a
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
  | Add_IntegerFF IntegerFF IntegerFF
  | Sub_IntegerFF IntegerFF IntegerFF
  | Ite_IntegerFF BoolFF IntegerFF IntegerFF
  | Var_IntegerFF FreeID
  | Unreachable_IntegerFF
  deriving (Show)

integerFF :: Integer -> IntegerFF
integerFF = IntegerFF

eq_IntegerFF :: IntegerFF -> IntegerFF -> BoolFF
eq_IntegerFF (IntegerFF a) (IntegerFF b) = boolFF (a == b)
eq_IntegerFF Unreachable_IntegerFF _ = BoolFF_Unreachable
eq_IntegerFF _ Unreachable_IntegerFF = BoolFF_Unreachable
eq_IntegerFF a b 
 | a `stableNameEq` b = trueFF
 | otherwise = IEqFF a b

leq_IntegerFF :: IntegerFF -> IntegerFF -> BoolFF
leq_IntegerFF (IntegerFF a) (IntegerFF b) = boolFF (a <= b)
leq_IntegerFF Unreachable_IntegerFF _ = BoolFF_Unreachable
leq_IntegerFF _ Unreachable_IntegerFF = BoolFF_Unreachable
leq_IntegerFF a b = ILeqFF a b

add_IntegerFF :: IntegerFF -> IntegerFF -> IntegerFF
add_IntegerFF (IntegerFF a) (IntegerFF b) = IntegerFF (a + b)
add_IntegerFF Unreachable_IntegerFF _ = Unreachable_IntegerFF
add_IntegerFF _ Unreachable_IntegerFF = Unreachable_IntegerFF
add_IntegerFF a b = Add_IntegerFF a b

sub_IntegerFF :: IntegerFF -> IntegerFF -> IntegerFF
sub_IntegerFF (IntegerFF a) (IntegerFF b) = IntegerFF (a - b)
sub_IntegerFF Unreachable_IntegerFF _ = Unreachable_IntegerFF
sub_IntegerFF _ Unreachable_IntegerFF = Unreachable_IntegerFF
sub_IntegerFF a b = Sub_IntegerFF a b

var_IntegerFF :: FreeID -> IntegerFF
var_IntegerFF = Var_IntegerFF

ite_IntegerFF :: BoolFF -> IntegerFF -> IntegerFF -> IntegerFF
ite_IntegerFF TrueFF a _ = a
ite_IntegerFF FalseFF _ b = b
ite_IntegerFF BoolFF_Unreachable _ _ = Unreachable_IntegerFF
ite_IntegerFF p v@(IntegerFF a) (IntegerFF b) | a == b = v
ite_IntegerFF _ Unreachable_IntegerFF b = b
ite_IntegerFF _ a Unreachable_IntegerFF = a
ite_IntegerFF p a b = Ite_IntegerFF p a b

instance Num IntegerFF where
  fromInteger = integerFF
  (+) = add_IntegerFF
  (-) = sub_IntegerFF
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
  | BitFF_Unreachable
     deriving (Show)

bitFF :: Bit -> BitFF
bitFF = BitFF

var_BitFF :: Integer -> FreeID -> BitFF
var_BitFF = Var_BitFF

eq_BitFF :: BitFF -> BitFF -> BoolFF
eq_BitFF (BitFF a) (BitFF b) = boolFF (a == b)
eq_BitFF BitFF_Unreachable _ = BoolFF_Unreachable
eq_BitFF _ BitFF_Unreachable = BoolFF_Unreachable
eq_BitFF a b = BitEqFF a b

leq_BitFF ::  BitFF -> BitFF -> BoolFF
leq_BitFF (BitFF a) (BitFF b) = boolFF (a <= b)
leq_BitFF BitFF_Unreachable _ = BoolFF_Unreachable
leq_BitFF _ BitFF_Unreachable = BoolFF_Unreachable
leq_BitFF a b = BitLeqFF a b

add_BitFF :: BitFF -> BitFF -> BitFF
add_BitFF (BitFF a) (BitFF b) = BitFF (a + b)
add_BitFF BitFF_Unreachable _ = BitFF_Unreachable
add_BitFF _ BitFF_Unreachable = BitFF_Unreachable
add_BitFF a b = Add_BitFF a b

sub_BitFF :: BitFF -> BitFF -> BitFF
sub_BitFF (BitFF a) (BitFF b) = BitFF (a - b)
sub_BitFF BitFF_Unreachable _ = BitFF_Unreachable
sub_BitFF _ BitFF_Unreachable = BitFF_Unreachable
sub_BitFF a b = Sub_BitFF a b

mul_BitFF :: BitFF -> BitFF -> BitFF
mul_BitFF (BitFF a) (BitFF b) = BitFF (a * b)
mul_BitFF BitFF_Unreachable _ = BitFF_Unreachable
mul_BitFF _ BitFF_Unreachable = BitFF_Unreachable
mul_BitFF a b = Mul_BitFF a b

bit_orFF :: BitFF -> BitFF -> BitFF
bit_orFF (BitFF a) (BitFF b) = BitFF (a .|. b)
bit_orFF BitFF_Unreachable _ = BitFF_Unreachable
bit_orFF _ BitFF_Unreachable = BitFF_Unreachable
bit_orFF a b = Or_BitFF a b

bit_andFF :: BitFF -> BitFF -> BitFF
bit_andFF (BitFF a) (BitFF b) = BitFF (a .&. b)
bit_andFF BitFF_Unreachable _ = BitFF_Unreachable
bit_andFF _ BitFF_Unreachable = BitFF_Unreachable
bit_andFF a b = And_BitFF a b

bit_shlFF :: Integer -> BitFF -> BitFF -> BitFF
bit_shlFF _ (BitFF a) (BitFF b) = BitFF (a `bv_shl` b)
bit_shlFF _ BitFF_Unreachable _ = BitFF_Unreachable
bit_shlFF _ _ BitFF_Unreachable = BitFF_Unreachable
bit_shlFF w a b = Shl_BitFF w a b

bit_lshrFF :: Integer -> BitFF -> BitFF -> BitFF
bit_lshrFF _ (BitFF a) (BitFF b) = BitFF (a `bv_lshr` b)
bit_lshrFF _ BitFF_Unreachable _ = BitFF_Unreachable
bit_lshrFF _ _ BitFF_Unreachable = BitFF_Unreachable
bit_lshrFF w a b = Lshr_BitFF w a b

bit_concatFF :: BitFF -> BitFF -> BitFF
bit_concatFF (BitFF a) (BitFF b) = BitFF (a `bv_concat` b)
bit_concatFF BitFF_Unreachable _ = BitFF_Unreachable
bit_concatFF _ BitFF_Unreachable = BitFF_Unreachable
bit_concatFF a b = Concat_BitFF a b

bit_notFF :: BitFF -> BitFF
bit_notFF (BitFF a) = BitFF (complement a)
bit_notFF BitFF_Unreachable = BitFF_Unreachable
bit_notFF a = Not_BitFF a

bit_sign_extendFF :: Integer -> Integer -> BitFF -> BitFF
bit_sign_extendFF fr to (BitFF a) = BitFF (bv_sign_extend (to-fr) a)
bit_sign_extendFF _ _ BitFF_Unreachable = BitFF_Unreachable
bit_sign_extendFF fr to x = SignExtend_BitFF fr to x

bit_extractFF :: Integer -> Integer -> BitFF -> BitFF
bit_extractFF hi lo (BitFF a) = BitFF (bv_extract hi lo a)
bit_extractFF _ _ BitFF_Unreachable = BitFF_Unreachable
bit_extractFF hi lo x = Extract_BitFF hi lo x

ite_BitFF :: BoolFF -> BitFF -> BitFF -> BitFF
ite_BitFF TrueFF a _ = a
ite_BitFF FalseFF _ b = b
ite_BitFF BoolFF_Unreachable _ _ = BitFF_Unreachable
ite_BitFF p v@(BitFF a) (BitFF b) | a == b = v
ite_BitFF _ BitFF_Unreachable b = b
ite_BitFF _ a BitFF_Unreachable = a
ite_BitFF p a b = Ite_BitFF p a b

