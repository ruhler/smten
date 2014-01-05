
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
  | IAddFF IntegerFF IntegerFF
  | ISubFF IntegerFF IntegerFF
  | IIteFF BoolFF IntegerFF IntegerFF
  | IVarFF FreeID
  | IntegerFF_Unreachable
  deriving (Show)

integerFF :: Integer -> IntegerFF
integerFF = IntegerFF

ieqFF :: IntegerFF -> IntegerFF -> BoolFF
ieqFF (IntegerFF a) (IntegerFF b) = boolFF (a == b)
ieqFF IntegerFF_Unreachable _ = BoolFF_Unreachable
ieqFF _ IntegerFF_Unreachable = BoolFF_Unreachable
ieqFF a b 
 | a `stableNameEq` b = trueFF
 | otherwise = IEqFF a b

ileqFF :: IntegerFF -> IntegerFF -> BoolFF
ileqFF (IntegerFF a) (IntegerFF b) = boolFF (a <= b)
ileqFF IntegerFF_Unreachable _ = BoolFF_Unreachable
ileqFF _ IntegerFF_Unreachable = BoolFF_Unreachable
ileqFF a b = ILeqFF a b

iaddFF :: IntegerFF -> IntegerFF -> IntegerFF
iaddFF (IntegerFF a) (IntegerFF b) = IntegerFF (a + b)
iaddFF IntegerFF_Unreachable _ = IntegerFF_Unreachable
iaddFF _ IntegerFF_Unreachable = IntegerFF_Unreachable
iaddFF a b = IAddFF a b

isubFF :: IntegerFF -> IntegerFF -> IntegerFF
isubFF (IntegerFF a) (IntegerFF b) = IntegerFF (a - b)
isubFF IntegerFF_Unreachable _ = IntegerFF_Unreachable
isubFF _ IntegerFF_Unreachable = IntegerFF_Unreachable
isubFF a b = ISubFF a b

ivarFF :: FreeID -> IntegerFF
ivarFF = IVarFF

iiteFF :: BoolFF -> IntegerFF -> IntegerFF -> IntegerFF
iiteFF TrueFF a _ = a
iiteFF FalseFF _ b = b
iiteFF BoolFF_Unreachable _ _ = IntegerFF_Unreachable
iiteFF p v@(IntegerFF a) (IntegerFF b) | a == b = v
iiteFF _ IntegerFF_Unreachable b = b
iiteFF _ a IntegerFF_Unreachable = a
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
  | BitFF_Unreachable
     deriving (Show)

bitFF :: Bit -> BitFF
bitFF = BitFF

bit_varFF :: Integer -> FreeID -> BitFF
bit_varFF = Var_BitFF

bit_eqFF :: BitFF -> BitFF -> BoolFF
bit_eqFF (BitFF a) (BitFF b) = boolFF (a == b)
bit_eqFF BitFF_Unreachable _ = BoolFF_Unreachable
bit_eqFF _ BitFF_Unreachable = BoolFF_Unreachable
bit_eqFF a b = BitEqFF a b

bit_leqFF ::  BitFF -> BitFF -> BoolFF
bit_leqFF (BitFF a) (BitFF b) = boolFF (a <= b)
bit_leqFF BitFF_Unreachable _ = BoolFF_Unreachable
bit_leqFF _ BitFF_Unreachable = BoolFF_Unreachable
bit_leqFF a b = BitLeqFF a b

bit_addFF :: BitFF -> BitFF -> BitFF
bit_addFF (BitFF a) (BitFF b) = BitFF (a + b)
bit_addFF BitFF_Unreachable _ = BitFF_Unreachable
bit_addFF _ BitFF_Unreachable = BitFF_Unreachable
bit_addFF a b = Add_BitFF a b

bit_subFF :: BitFF -> BitFF -> BitFF
bit_subFF (BitFF a) (BitFF b) = BitFF (a - b)
bit_subFF BitFF_Unreachable _ = BitFF_Unreachable
bit_subFF _ BitFF_Unreachable = BitFF_Unreachable
bit_subFF a b = Sub_BitFF a b

bit_mulFF :: BitFF -> BitFF -> BitFF
bit_mulFF (BitFF a) (BitFF b) = BitFF (a * b)
bit_mulFF BitFF_Unreachable _ = BitFF_Unreachable
bit_mulFF _ BitFF_Unreachable = BitFF_Unreachable
bit_mulFF a b = Mul_BitFF a b

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

bit_iteFF :: BoolFF -> BitFF -> BitFF -> BitFF
bit_iteFF TrueFF a _ = a
bit_iteFF FalseFF _ b = b
bit_iteFF BoolFF_Unreachable _ _ = BitFF_Unreachable
bit_iteFF p v@(BitFF a) (BitFF b) | a == b = v
bit_iteFF _ BitFF_Unreachable b = b
bit_iteFF _ a BitFF_Unreachable = a
bit_iteFF p a b = Ite_BitFF p a b

