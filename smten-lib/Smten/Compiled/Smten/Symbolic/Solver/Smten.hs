
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic.Solver.Smten (smten) where

import Control.Monad
import qualified Data.Map as Map

import Data.IORef
import Data.Functor
import Data.Maybe

import Smten.Runtime.Types (Type(..))
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.Bits
import Smten.Runtime.Integers

-- A clause represents a conjunction of literals.
-- A variable is present in the conjunction if it is in the Clause map. A
-- value of True means it is a positive literal, a value of False means it is
-- a negative literal.
type Clause = Map.Map String Bool

-- Disjunctive normal form for a boolean formula. Represents the disjunction
-- of a bunch of clauses.
type DNF = [Clause]

data Exp = Exp {
    pos :: DNF, -- DNF describing how to make this expression true.
    neg :: DNF  -- DNF describing how to make this expression false.
}

data SmtenSolver = SmtenSolver (IORef Exp)

nobits = error "SmtenSolver: bits not supported natively"
noints = error "SmtenSolver: integers not supported natively"

instance SolverAST SmtenSolver Exp where
  declare (SmtenSolver mref) BoolT nm = return ()
  declare (SmtenSolver mref) IntegerT nm = noints
  declare (SmtenSolver mref) (BitT _) nm = nobits

  getBoolValue (SmtenSolver mref) nm = do
     ms <- readIORef mref
     case pos ms of
        [] -> error $ "getBoolValue called when there is no model"
        (m:_) -> return (fromMaybe False (Map.lookup nm m))
  
  getIntegerValue = noints
  getBitVectorValue = nobits

  check (SmtenSolver mref) = do
     f <- pos <$> readIORef mref
     return (if null f then Unsat else Sat)

  assert (SmtenSolver mref) p = modifyIORef mref $ andExp p

  bool _ True = return trueExp
  bool _ False = return falseExp
  integer = noints
  bit = nobits

  var _ nm = return $ varExp nm

  and_bool _ a b = return $ andExp a b
  not_bool _ a = return $ notExp a
  ite_bool _ p a b = return $ iteExp p a b

  ite_integer = noints
  ite_bit _ = nobits

  eq_integer = noints
  leq_integer = noints
  add_integer = noints
  sub_integer = noints

  eq_bit = nobits
  leq_bit = nobits
  add_bit = nobits
  sub_bit = nobits
  mul_bit = nobits
  or_bit = nobits
  and_bit = nobits
  shl_bit = nobits
  lshr_bit = nobits
  concat_bit = nobits
  not_bit = nobits
  sign_extend_bit = nobits
  extract_bit = nobits

smten :: Solver
smten = do
   mref <- newIORef trueExp
   let base = SmtenSolver mref
   withints <- addIntegers base
   withbits <- addBits withints
   return $ solverInstFromAST withbits

trueExp :: Exp
trueExp = Exp trueDNF falseDNF

falseExp :: Exp
falseExp = Exp falseDNF trueDNF

varExp :: String -> Exp
varExp nm = Exp {
    pos = [Map.singleton nm True],
    neg = [Map.singleton nm False]
}

notExp :: Exp -> Exp
notExp (Exp p n) = Exp n p

orExp :: Exp -> Exp -> Exp
orExp (Exp ap an) (Exp bp bn) = Exp {
    pos = orDNF ap bp,
    neg = andDNF an bn
}

andExp :: Exp -> Exp -> Exp
andExp (Exp ap an) (Exp bp bn) = Exp {
    pos = andDNF ap bp,
    neg = orDNF an bn
}

iteExp :: Exp -> Exp -> Exp -> Exp
iteExp p a b = orExp (andExp p a) (andExp (notExp p) b)

trueDNF :: DNF
trueDNF = {-# SCC "trueDNF" #-} [Map.empty]

falseDNF :: DNF
falseDNF = {-# SCC "falseDNF" #-} []

orDNF :: DNF -> DNF -> DNF
orDNF = {-# SCC "orDNF" #-} (++)

andDNF :: DNF -> DNF -> DNF
andDNF a b = {-# SCC "andDNF" #-} do
  am <- a
  bm <- b
  guard $ nonConflicting am bm
  return $ am `Map.union` bm

nonConflicting :: Clause -> Clause -> Bool
nonConflicting a b = {-# SCC "nonConflicting" #-}
  all (\(ak, av) -> av == fromMaybe av (Map.lookup ak b)) (Map.assocs a)

