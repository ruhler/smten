
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic0 (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic
    ) where

import Control.Monad.State
import Data.Dynamic
import Data.Functor((<$>))

import Smten.Runtime.ErrorString
import Smten.Runtime.FreeID
import Smten.Runtime.Formula
import Smten.Runtime.Model
import Smten.Runtime.Result
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver

data SS = SS {
    ss_pred :: BoolF,
    ss_free :: [(FreeID, TypeF)],
    ss_formula :: BoolF
}

type Symbolic = StateT SS IO

instance SmtenHS1 Symbolic where
    error1 msg = return (error0 msg)

    ite1 p a b = do
      va <- predicated p a
      vb <- predicated (notF p) b
      return (ite p va vb)

    realize1 m x = realize m <$> x

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = do
    modify $ \ss -> ss { ss_formula = ss_formula ss `andF` notF (ss_pred ss) }
    return (error0 (errstr "mzero_symbolic"))

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, BoolTF) : ss_free s }
    ite0 (VarF fid) a b

predicated :: BoolF -> Symbolic a -> Symbolic a
predicated p q = do
    pold <- gets ss_pred
    modify $ \ss -> ss { ss_pred = pold `andF` p }
    v <- q
    modify $ \ss -> ss { ss_pred = pold }
    return v

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (Maybe a)
run_symbolic s q = do
  solver <- s
  (x, ss) <- runStateT q (SS TrueF [] TrueF)
  mapM_ (declVar solver) (ss_free ss)
  assert solver (ss_formula ss)
  res <- check solver
  case res of
    Sat -> do
       let vars = ss_free ss
       vals <- mapM (getValue solver) vars
       m <- model $ zip (map fst vars) vals
       case {-# SCC "DoubleCheck" #-} realize m (ss_formula ss) of
          TrueF -> return ()
          x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied? " ++ show x
       return (Just ({-# SCC "Realize" #-} realize m x))
    Unsat -> return Nothing

getValue :: SolverInst -> (FreeID, TypeF) -> IO AnyF
getValue s (f, BoolTF) = do
   b <- getBoolValue s (freenm f)
   return (BoolF $ if b then TrueF else FalseF)

declVar :: SolverInst -> (FreeID, TypeF) -> IO ()
declVar s (nm, ty) = declare s ty (freenm nm)
