
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Runtime.Symbolic (
    Symbolic, return_symbolic, bind_symbolic, run_symbolic,
    fail_symbolic, free_Bool, free_Integer,
    IO, Maybe, Solver, R.Bool, R.Integer,
    ) where

import Control.Monad.State
import Data.Dynamic
import Data.Functor((<$>))

import Smten.Symbolic
import qualified Smten.SMT.Solver as SMT
import Smten.Runtime.SmtenHS
import qualified Smten.Runtime.Prelude as R
import Smten.SMT.FreeID
import Smten.SMT.Yices.Yices2
import Smten.SMT.DebugLL

data SS = SS {
    ss_pred :: R.Bool,
    ss_free :: [(FreeID, SMTType)],
    ss_formula :: R.Bool
}

type Symbolic = StateT SS IO

data SMTType = SMTBool | SMTInteger 
    deriving (Eq, Show)
    
instance (Haskelly ha sa) => Haskelly (Symbolic ha) (Symbolic sa) where
    frhs x = frhs <$> x
    tohs x = return (tohs' <$> x)

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

free_Bool :: Symbolic R.Bool
free_Bool = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTBool) : ss_free s }
    return $ R.BoolVar fid

free_Integer :: Symbolic R.Integer
free_Integer = do
    fid <- liftIO fresh
    modify $ \s -> s { ss_free = (fid, SMTInteger) : ss_free s }
    return $ R.IntegerVar fid

predicated :: R.Bool -> Symbolic a -> Symbolic a
predicated p q = do
    pold <- gets ss_pred
    modify $ \ss -> ss { ss_pred = pold `andB` p }
    v <- q
    modify $ \ss -> ss { ss_pred = pold }
    return v

mksolver :: Solver -> IO (SMT.Solver)
mksolver Yices2 = yices2
mksolver (DebugLL dbg s) = do
    s' <- mksolver s
    debugll dbg s'
mksolver d = error $ "TODO: mksolver: " ++ show d

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (Maybe a)
run_symbolic s q = do
  solver <- mksolver s
  (x, ss) <- runStateT q (SS R.True [] R.True)
  mapM_ (declare solver) (ss_free ss)
  SMT.assert solver (ss_formula ss)
  res <- SMT.check solver
  case res of
    SMT.Satisfiable -> do
       let vars = ss_free ss
       vals <- mapM (getValue solver) vars
       return (Just (realize0 (zip (map fst vars) vals) x))
    SMT.Unsatisfiable -> return Nothing

declare :: SMT.Solver -> (FreeID, SMTType) -> IO ()
declare s (f, SMTBool) = SMT.declare_bool s (freenm f)
declare s (f, SMTInteger) = SMT.declare_integer s (freenm f)

getValue :: SMT.Solver -> (FreeID, SMTType) -> IO Dynamic
getValue s (f, SMTBool) = toDyn <$> SMT.getBoolValue s (freenm f)
getValue s (f, SMTInteger) = toDyn <$> SMT.getIntegerValue s (freenm f)
 
andB :: R.Bool -> R.Bool -> R.Bool
andB p q = R.__caseTrue p q R.False

notB :: R.Bool -> R.Bool
notB p = R.__caseTrue p R.False R.True

