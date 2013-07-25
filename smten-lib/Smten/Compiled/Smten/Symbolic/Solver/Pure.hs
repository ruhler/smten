
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Compiled.Smten.Symbolic.Solver.Pure (pure) where

import Control.Monad

import Data.IORef
import Data.Functor

import Smten.Runtime.Types (Type(..))
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Any = B Bool
         | I Integer
    deriving (Eq)

data Model = Model {
    vars :: [(String, Any)]
}

data Exp = Exp (Model -> Any)

data PureSolver = PureSolver (IORef [Model])

instance SolverAST PureSolver Exp where
  declare (PureSolver mref) BoolT nm = do
     ms <- readIORef mref
     writeIORef mref $ do
        vs <- vars <$> ms
        b <- [True, False]
        return (Model $ (nm, B b) : vs)

  -- For now give an error in this case.
  -- We could just return the list of all integers and hope for the best, but
  -- for debugging purposes, because I plan not to use that feature, throw an
  -- error if that is attempted.
  declare (PureSolver mref) IntegerT nm = error "PureSolver.free_Integer"

  getBoolValue (PureSolver mref) nm = do
     ms <- readIORef mref
     case ms of
        [] -> error $ "getBoolValue called when there is no model"
        (x:_) ->
          case lookup nm (vars x) of
            Just (B b) -> return b
            Nothing -> error $ "getBoolValue: " ++ nm ++ " not found"
  
  getIntegerValue = error "PureSolver.getIntegerValue"

  check (PureSolver mref) = do
     ms <- readIORef mref
     return (if null ms then Unsat else Sat)

  assert (PureSolver mref) (Exp p) = do
     ms <- readIORef mref
     writeIORef mref $ do
        m <- ms
        guard (p m == B True)
        return m

  bool _ x = return (Exp $ const (B x))
  integer _ x = return (Exp $ const (I x))

  var _ nm = return . Exp $ \m ->
                case lookup nm (vars m) of
                    Just x -> x
                    Nothing -> error $ "var: " ++ nm ++ " not found"

  and_bool ctx a b = ite_bool ctx a b (Exp $ const (B False))
  not_bool ctx a = ite_bool ctx a (Exp $ const (B False)) (Exp $ const (B True))

  ite_bool _ (Exp p) (Exp a) (Exp b) = return . Exp $ \m ->
     case p m of
        B True -> a m
        B False -> b m

  ite_integer _ (Exp p) (Exp a) (Exp b) = return . Exp $ \m ->
     case p m of
        B True -> a m
        B False -> b m

  eq_integer _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (I av, I bv) -> B (av == bv)

  leq_integer _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (I av, I bv) -> B (av <= bv)

  add_integer _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (I av, I bv) -> I (av + bv)

  sub_integer _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (I av, I bv) -> I (av - bv)

pure :: Solver
pure = do
   mref <- newIORef [Model []]
   return $ solverInstFromAST (PureSolver mref)

