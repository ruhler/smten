
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic.Solver.Smten (smten) where

import Control.Monad
import qualified Data.Map as Map

import Data.IORef
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

data SmtenSolver = SmtenSolver (IORef DNF)

nobits = error "SmtenSolver: bits not supported natively"
noints = error "SmtenSolver: integers not supported natively"

instance SolverAST SmtenSolver DNF where
  declare (SmtenSolver mref) BoolT nm = return ()
  declare (SmtenSolver mref) IntegerT nm = noints
  declare (SmtenSolver mref) (BitT _) nm = nobits

  getBoolValue (SmtenSolver mref) nm = do
     ms <- readIORef mref
     case ms of
        [] -> error $ "getBoolValue called when there is no model"
        (m:_) -> return (fromMaybe False (Map.lookup nm m))
  
  getIntegerValue = noints
  getBitVectorValue = nobits

  check (SmtenSolver mref) = do
     f <- readIORef mref
     return (if null f then Unsat else Sat)

  assert (SmtenSolver mref) p = modifyIORef mref $ andDNF p

  bool _ True = return trueDNF
  bool _ False = return falseDNF
  integer = noints
  bit = nobits

  var _ nm = return $ varDNF nm

  and_bool _ a b = return $ andDNF a b
  not_bool _ a = return $ notDNF a
  ite_bool _ p a b = return $ iteDNF p a b

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
   mref <- newIORef trueDNF
   let base = SmtenSolver mref
   withints <- addIntegers base
   withbits <- addBits withints
   return $ solverInstFromAST withbits


trueDNF :: DNF
trueDNF = {-# SCC "trueDNF" #-} [Map.empty]

falseDNF :: DNF
falseDNF = {-# SCC "falseDNF" #-} []

varDNF :: String -> DNF
varDNF nm = {-# SCC "varDNF" #-} [Map.singleton nm True]

notDNF :: DNF -> DNF
notDNF xs = foldr andDNF trueDNF $ map notClause xs

notClause :: Clause -> DNF
notClause m = {-# SCC "notClause" #-}
    [Map.singleton nm (not v) | (nm, v) <- Map.assocs m]

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

iteDNF :: DNF -> DNF -> DNF -> DNF
iteDNF p a b = {-# SCC "iteDNF" #-} orDNF (andDNF p a) (andDNF (notDNF p) b)

