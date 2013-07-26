
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Compiled.Smten.Symbolic.Solver.Pure (pure) where

import Control.Monad

import Data.Bits
import Data.IORef
import Data.Functor

import Smten.Runtime.Bit
import Smten.Runtime.Types (Type(..))
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Any = B Bool
         | I Integer
         | BV Bit
    deriving (Eq)

data Model = Model {
    vars :: [(String, Any)]
}

data Exp = Exp (Model -> Any)

data PureSolver = PureSolver (IORef [Model])

ite_Exp :: Exp -> Exp -> Exp -> IO Exp
ite_Exp (Exp p) (Exp a) (Exp b) = return . Exp $ \m ->
     case p m of
        B True -> a m
        B False -> b m

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
  declare (PureSolver mref) IntegerT nm = error "PureSolver.declare Integer"
  declare (PureSolver mref) (BitT _) nm = error "PureSolver.declare Bit"

  getBoolValue (PureSolver mref) nm = do
     ms <- readIORef mref
     case ms of
        [] -> error $ "getBoolValue called when there is no model"
        (x:_) ->
          case lookup nm (vars x) of
            Just (B b) -> return b
            Nothing -> error $ "getBoolValue: " ++ nm ++ " not found"
  
  getIntegerValue = error "PureSolver.getIntegerValue"
  getBitVectorValue = error "PureSolver.getBitVectorValue"

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
  bit _ w v = return (Exp $ const (BV (bv_make w v)))

  var _ nm = return . Exp $ \m ->
                case lookup nm (vars m) of
                    Just x -> x
                    Nothing -> error $ "var: " ++ nm ++ " not found"

  and_bool ctx a b = ite_bool ctx a b (Exp $ const (B False))
  not_bool ctx a = ite_bool ctx a (Exp $ const (B False)) (Exp $ const (B True))

  ite_bool _ = ite_Exp
  ite_integer _ = ite_Exp
  ite_bit _ = ite_Exp

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

  eq_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> B (av == bv)

  leq_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> B (av <= bv)

  add_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av + bv)

  sub_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av - bv)

  mul_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av * bv)

  or_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av .|. bv)

  and_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av .&. bv)

  shl_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av `bv_shl` bv)

  lshr_bit _ (Exp a) (Exp b) = return . Exp $ \m ->
     case (a m, b m) of
        (BV av, BV bv) -> BV (av `bv_lshr` bv)

  not_bit _ (Exp a) = return . Exp $ \m ->
     case a m of
        BV av -> BV (complement av)

pure :: Solver
pure = do
   mref <- newIORef [Model []]
   return $ solverInstFromAST (PureSolver mref)

