
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic0 (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Control.Monad.State
import Data.Functor((<$>))

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Types hiding (Integer)
import Smten.Runtime.Result
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver

import Smten.Compiled.Smten.Data.Bit0 as S
import Smten.Compiled.Smten.Data.Maybe as S
import qualified Smten.Compiled.Smten.Smten.Integer as S
import qualified Smten.Runtime.Types as S

data SS = SS {
    ss_pred :: S.Bool,
    ss_free :: [(FreeID, Type)],
    ss_formula :: S.Bool
}

type Symbolic = StateT SS IO

instance SmtenHS1 Symbolic where
    error1 msg = return (error0 msg)

    ite1 p a b = do
      va <- predicated p a
      vb <- predicated (notB p) b
      return (ite p va vb)

    realize1 m x = realize m <$> x

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = do
    modify $ \ss -> ss { ss_formula = ss_formula ss `andB` notB (ss_pred ss) }
    return (error0 (errstr "mzero_symbolic"))

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, BoolT) : ss_free s }
    ite0 (S.Bool_Var fid) a b

free_Integer :: Symbolic (S.Integer)
free_Integer = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, IntegerT) : ss_free s }
    return (S.Integer_Var fid)

free_Bit :: Integer -> Symbolic (S.Bit n)
free_Bit w = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, BitT w) : ss_free s }
    return (S.Bit_Var fid)

predicated :: S.Bool -> Symbolic a -> Symbolic a
predicated p q = do
    pold <- gets ss_pred
    modify $ \ss -> ss { ss_pred = pold `andB` p }
    v <- q
    modify $ \ss -> ss { ss_pred = pold }
    return v

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  solver <- s
  (x, ss) <- runStateT q (SS S.True [] S.True)
  mapM_ (declVar solver) (ss_free ss)
  assert solver (ss_formula ss)
  res <- check solver
  case res of
    Sat -> do
       let vars = ss_free ss
       vals <- mapM (getValue solver) vars
       m <- model $ zip (map fst vars) vals
       case {-# SCC "DoubleCheck" #-} realize m (ss_formula ss) of
          S.True -> return ()
          x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied?"
       return (S.Just ({-# SCC "Realize" #-} realize m x))
    Unsat -> return S.Nothing

getValue :: SolverInst -> (FreeID, Type) -> IO Any
getValue s (f, BoolT) = do
   b <- getBoolValue s (freenm f)
   return (BoolA $ if b then S.True else S.False)
getValue s (f, IntegerT) = do
   b <- getIntegerValue s (freenm f)
   return (IntegerA $ S.Integer b)
getValue s (f, BitT w) = do
   b <- getBitVectorValue s w (freenm f)
   return (BitA $ bv_make w b)

declVar :: SolverInst -> (FreeID, Type) -> IO ()
declVar s (nm, ty) = declare s ty (freenm nm)

