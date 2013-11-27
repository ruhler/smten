
-- | Representation of a finite SMT Formula
module Smten.Runtime.Formula.Finite (
  BoolFF(..), trueFF, falseFF, boolFF, andFF, orFF, notFF, iteFF, varFF,
  IntegerFF(..), integerFF, iaddFF, isubFF, iiteFF, ivarFF, ieqFF, ileqFF,
  ) where

import Smten.Runtime.FreeID

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
  deriving (Eq, Show)

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
andFF FalseFF _ = FalseFF
andFF a TrueFF = a
andFF _ FalseFF = FalseFF
andFF a b
 | a == b = a
 | otherwise = AndFF a b
            
notFF :: BoolFF -> BoolFF
notFF TrueFF = FalseFF
notFF FalseFF = TrueFF
notFF (IteFF p a b) = IteFF p b a
notFF (NotFF x) = x
notFF x = NotFF x

orFF :: BoolFF -> BoolFF -> BoolFF
orFF TrueFF _ = trueFF
orFF _ TrueFF = trueFF
orFF FalseFF b = b
orFF a FalseFF = a
orFF a b
 | a == b = a
 | otherwise = OrFF a b

iteFF :: BoolFF -> BoolFF -> BoolFF -> BoolFF
iteFF TrueFF a _ = a
iteFF FalseFF _ b = b
iteFF p TrueFF b = orFF p b
iteFF p a FalseFF = andFF p a
iteFF p a b 
 | a == b = a
 | otherwise = IteFF p a b

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
  deriving (Show, Eq)

integerFF :: Integer -> IntegerFF
integerFF = IntegerFF

ieqFF :: IntegerFF -> IntegerFF -> BoolFF
ieqFF (IntegerFF a) (IntegerFF b) = if a == b then trueFF else falseFF
ieqFF a b 
 | a == b = trueFF
 | otherwise = IEqFF a b

ileqFF :: IntegerFF -> IntegerFF -> BoolFF
ileqFF (IntegerFF a) (IntegerFF b) = if a <= b then trueFF else falseFF
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
