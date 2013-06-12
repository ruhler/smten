
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Runtime.Symbolic (
    Symbolic,
    return_symbolic, bind_symbolic, run_symbolic,
    fail_symbolic, free_Bool, free_Integer, free_Bit,
    IO, Maybe, Solvers.Solver, S.Bool, S.Integer, S.Bit,
    ) where

import Prelude as P

import Control.Monad.State
import Data.Dynamic
import Data.Functor((<$>))

import Smten.Bit
import Smten.SMT.Solver.Dynamic
import qualified Smten.SMT.Solvers as Solvers
import Smten.Runtime.SmtenHS as S
import Smten.SMT.FreeID

data SS = SS {
    ss_pred :: S.Bool,
    ss_free :: [(FreeID, SMTType)],
    ss_formula :: S.Bool
}

type Symbolic = StateT SS IO

data SMTType = SMTBool | SMTInteger | SMTBit P.Integer
    deriving (Eq, Show)
    
instance (Haskelly ha sa) => Haskelly (Symbolic ha) (Symbolic sa) where
    frhs x = frhs <$> x
    stohs x = stohs <$> x

instance Haskelly (Symbolic a) (Symbolic a) where
    frhs = id
    stohs = id

instance SmtenHS1 Symbolic where
    realize1 m x = realize0 m <$> x
    cases1 x = concrete x
    primitive1 _ (Concrete x) = x
    primitive1 _ (Switch p a b) = do
      va <- predicated p (primitive0 (error "Symbolic.primitive1") a)
      vb <- predicated (notB p) (primitive0 (error "Symbolic.primitive1") b)
      return (__caseTrue p va vb)
    
return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

fail_symbolic :: Symbolic a
fail_symbolic = do
    modify $ \ss -> ss { ss_formula = ss_formula ss `andB` notB (ss_pred ss) }
    return (error "fail_symbolic")

free_Bool :: Symbolic S.Bool
free_Bool = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTBool) : ss_free s }
    return $ S.Bool_Var fid

free_Integer :: Symbolic S.Integer
free_Integer = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTInteger) : ss_free s }
    return $ S.Integer_Var fid

free_Bit :: S.Integer -> Symbolic S.Bit
free_Bit (S.Integer v) = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTBit v) : ss_free s }
    return $ S.Bit_Var fid

predicated :: S.Bool -> Symbolic a -> Symbolic a
predicated p q = do
    pold <- gets ss_pred
    modify $ \ss -> ss { ss_pred = pold `andB` p }
    v <- q
    modify $ \ss -> ss { ss_pred = pold }
    return v

run_symbolic :: (SmtenHS0 a) => Solvers.Solver -> Symbolic a -> IO (Maybe a)
run_symbolic s q = do
  solver <- Solvers.mkSolver s
  (x, ss) <- runStateT q (SS S.True [] S.True)
  mapM_ (declare solver) (ss_free ss)
  assert solver (ss_formula ss)
  res <- check solver
  case res of
    Satisfiable -> do
       let vars = ss_free ss
       vals <- mapM (getValue solver) vars
       return (Just (realize0 (zip (map fst vars) vals) x))
    Unsatisfiable -> return Nothing

declare :: Solver -> (FreeID, SMTType) -> IO ()
declare s (f, SMTBool) = declare_bool s (freenm f)
declare s (f, SMTInteger) = declare_integer s (freenm f)
declare s (f, SMTBit w) = declare_bit s (freenm f) w

getValue :: Solver -> (FreeID, SMTType) -> IO Dynamic
getValue s (f, SMTBool) = toDyn <$> getBoolValue s (freenm f)
getValue s (f, SMTInteger) = toDyn <$> getIntegerValue s (freenm f)
getValue s (f, SMTBit w) = do
    v <- getBitVectorValue s (freenm f) w
    return $ toDyn (bv_make w v)
 
andB :: S.Bool -> S.Bool -> S.Bool
andB p q = S.__caseTrue p q S.False

notB :: S.Bool -> S.Bool
notB p = S.__caseTrue p S.False S.True

