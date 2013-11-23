
-- | Representation of a finite SMT Formula
module Smten.Runtime.FiniteFormula (
  FreeID,
  BoolFF(..), trueFF, falseFF, andFF, orFF, notFF, iteFF, varFF,
  ) where

type FreeID = String

-- | A boolean finite formula which contains no _|_.
data BoolFF =
   TrueFF
 | FalseFF
 | IteFF BoolFF BoolFF BoolFF
 | AndFF BoolFF BoolFF
 | OrFF BoolFF BoolFF
 | NotFF BoolFF
 | VarFF FreeID
  deriving (Eq, Show)

trueFF :: BoolFF
trueFF = TrueFF

falseFF :: BoolFF
falseFF = FalseFF

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
  


