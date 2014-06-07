
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Compiled.Smten.Search.Solver.Smten (smten) where

import Data.IORef
import Data.Functor

import Smten.Runtime.FreeID
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver
import Smten.Runtime.Bits
import Smten.Runtime.Integers

-- BDD: represents a set of clauses in disjunctive normal form.
-- The following invarient should hold for a BDD:
--    Var x is a parent of Var y if x < y.
data BDD = None     -- ^ unsatisfiable
         | Done     -- ^ valid (all assignments satisfy)
         | Var {
    _nm :: FreeID,  -- ^ The variable name
    _tt :: BDD,     -- ^ If the variable is true
    _ff :: BDD      -- ^ If the variable is false
  }  -- ^ Split on a variable

data Exp = Exp {
    pos :: BDD, -- BDD describing how to make this expression true.
    neg :: BDD  -- BDD describing how to make this expression false.
}

data SmtenSolver = SmtenSolver (IORef Exp)

nobits = error "SmtenSolver: bits not supported natively"
noints = error "SmtenSolver: integers not supported natively"

instance SolverAST SmtenSolver Exp () () where
  declare_bool (SmtenSolver mref) nm = return ()
  declare_integer (SmtenSolver mref) nm = noints
  declare_bit (SmtenSolver mref) _ nm = nobits

  getBoolValue (SmtenSolver mref) nm = do
     ms <- readIORef mref
     return (lookupBDD nm $ pos ms)
  
  getIntegerValue = noints
  getBitVectorValue = nobits

  check (SmtenSolver mref) = do
     f <- pos <$> readIORef mref
     return $ case f of
                 None -> Unsat
                 _ -> Sat

  assert (SmtenSolver mref) p = modifyIORef mref $ andExp p

  bool _ True = return trueExp
  bool _ False = return falseExp
  integer = noints
  bit = nobits

  var_bool _ nm = return $ varExp nm
  var_integer _ nm = noints
  var_bit _ w nm = nobits

  and_bool _ a b = return $ andExp a b
  or_bool _ a b = return $ orExp a b
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
  sdiv_bit = nobits
  srem_bit = nobits
  smod_bit = nobits
  udiv_bit = nobits
  urem_bit = nobits
  or_bit = nobits
  and_bit = nobits
  shl_bit = nobits
  lshr_bit = nobits
  concat_bit = nobits
  not_bit = nobits
  sign_extend_bit = nobits
  extract_bit = nobits

smten :: Solver
smten = solverFromAST $ do
   mref <- newIORef trueExp
   let base = SmtenSolver mref
   withints <- addIntegers base
   addBits withints

trueExp :: Exp
trueExp = Exp trueBDD falseBDD

falseExp :: Exp
falseExp = Exp falseBDD trueBDD

varExp :: FreeID -> Exp
varExp nm = Exp {
    pos = Var nm Done None,
    neg = Var nm None Done
}

notExp :: Exp -> Exp
notExp (Exp p n) = Exp n p

orExp :: Exp -> Exp -> Exp
orExp (Exp ap an) (Exp bp bn) = Exp {
    pos = orBDD ap bp,
    neg = andBDD an bn
}

andExp :: Exp -> Exp -> Exp
andExp (Exp ap an) (Exp bp bn) = Exp {
    pos = andBDD ap bp,
    neg = orBDD an bn
}

iteExp :: Exp -> Exp -> Exp -> Exp
iteExp p a b = orExp (andExp p a) (andExp (notExp p) b)

trueBDD :: BDD
trueBDD = Done

falseBDD :: BDD
falseBDD = None

orBDD :: BDD -> BDD -> BDD
orBDD None x = x
orBDD Done _ = Done
orBDD x None = x
orBDD x Done = Done
orBDD xa@(Var x xt xf) ya@(Var y yt yf)
  | x < y = Var x (orBDD xt ya) (orBDD xf ya)
  | x == y = Var x (orBDD xt yt) (orBDD xf yf)
  | otherwise = Var y (orBDD xa yt) (orBDD xa yf)

andBDD :: BDD -> BDD -> BDD
andBDD None x = None
andBDD Done x = x
andBDD x None = None
andBDD x Done = x
andBDD xa@(Var x xt xf) ya@(Var y yt yf)
  | x < y = case (andBDD xt ya, andBDD xf ya) of
               (None, None) -> None
               (t, f) -> Var x t f
  | x == y = case (andBDD xt yt, andBDD xf yf) of
               (None, None) -> None
               (t, f) -> Var x t f
  | otherwise = case (andBDD xa yt, andBDD xa yf) of
               (None, None) -> None
               (t, f) -> Var y t f

lookupBDD :: FreeID -> BDD -> Bool
lookupBDD nm None = error $ "no possible assignment for " ++ freenm nm
lookupBDD nm Done = False    -- Doesn't matter, just pick false.
lookupBDD nm (Var x None f)
  | nm <= x = False
  | otherwise = lookupBDD nm f
lookupBDD nm (Var x t _)
  | nm <= x = True
  | otherwise = lookupBDD nm t
    
