
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Runtime.Symbolic (
    Symbolic,
    return_symbolic, bind_symbolic, run_symbolic,
    fail_symbolic, free_Bool, free_Integer,
    IO, Maybe, Solvers.Solver, S.Bool, S.Integer,
    ) where

import Control.Monad.State
import Data.Dynamic
import Data.Functor((<$>))

import Smten.SMT.Solver
import qualified Smten.SMT.Solvers as Solvers
import Smten.Runtime.SmtenHS as S
import Smten.SMT.FreeID

data SS = SS {
    ss_pred :: S.Bool,
    ss_free :: [(FreeID, SMTType)],
    ss_formula :: S.Bool
}

type Symbolic = StateT SS IO

data SMTType = SMTBool | SMTInteger 
    deriving (Eq, Show)
    
instance (Haskelly ha sa) => Haskelly (Symbolic ha) (Symbolic sa) where
    frhs x = frhs <$> x
    tohs x = tohs <$> x

instance SmtenHS1 Symbolic where
    mux1 p a b = do
        va <- predicated p a
        vb <- predicated (notB p) b
        return (mux0 p va vb)

    realize1 m x = realize0 m <$> x
    strict_app1 f s = f s
      

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
    return $ S.BoolVar fid

free_Integer :: Symbolic S.Integer
free_Integer = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTInteger) : ss_free s }
    return $ S.IntegerVar fid

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

getValue :: Solver -> (FreeID, SMTType) -> IO Dynamic
getValue s (f, SMTBool) = toDyn <$> getBoolValue s (freenm f)
getValue s (f, SMTInteger) = toDyn <$> getIntegerValue s (freenm f)
 
andB :: S.Bool -> S.Bool -> S.Bool
andB p q = S.__caseTrue p q S.False

notB :: S.Bool -> S.Bool
notB p = S.__caseTrue p S.False S.True

