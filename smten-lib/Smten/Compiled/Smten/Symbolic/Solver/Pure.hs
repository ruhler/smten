
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Compiled.Smten.Symbolic.Solver.Pure (pure) where

import Control.Monad

import Data.IORef
import Data.Functor

import Smten.Runtime.Formula
import Smten.Runtime.Result
import Smten.Runtime.SolverAST
import Smten.Runtime.Solver

data Any = Bool Bool
    deriving (Eq)

data Model = Model {
    vars :: [(String, Any)]
}

data Exp = Exp (Model -> Any)

data PureSolver = PureSolver {
    models :: IORef [Model]
}

instance SolverAST PureSolver Exp where
  declare (PureSolver mref) BoolTF nm = do
     ms <- readIORef mref
     writeIORef mref $ do
        vs <- vars <$> ms
        b <- [True, False]
        return (Model $ (nm, Bool b) : vs)

  getBoolValue (PureSolver mref) nm = do
     ms <- readIORef mref
     case ms of
        [] -> error $ "getBoolValue called when there is no model"
        (x:_) ->
          case lookup nm (vars x) of
            Just (Bool b) -> return b
            Just _ -> error $ "getBoolValue: " ++ nm ++ " is not a bool var"
            Nothing -> error $ "getBoolValue: " ++ nm ++ " not found"

  check (PureSolver mref) = do
     ms <- readIORef mref
     return (if null ms then Unsat else Sat)

  assert (PureSolver mref) (Exp p) = do
     ms <- readIORef mref
     writeIORef mref $ do
        m <- ms
        guard (p m == Bool True)
        return m

  bool _ x = return (Exp $ const (Bool x))

  var _ nm = return . Exp $ \m ->
                case lookup nm (vars m) of
                    Just x -> x
                    Nothing -> error $ "var: " ++ nm ++ " not found"

  ite_bool _ (Exp p) (Exp a) (Exp b) = return . Exp $ \m ->
     case p m of
        Bool True -> a m
        Bool False -> b m
        

pure :: Solver
pure = do
   mref <- newIORef [Model []]
   return $ solverInstFromAST (PureSolver mref)

