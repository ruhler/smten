
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

module Smten.Compiled.Smten.Symbolic0 (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Prelude as P
import Data.Functor

import Smten.Runtime.FreeID
import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver
import Smten.Runtime.Model

import qualified Smten.Compiled.Smten.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits

data Symbolic a = Symbolic {
    -- Run the symbolic computation, producing an SMT formula and symbolic
    -- representation of the result.
    runS :: Fresh (BoolF, a),

    -- Realize the symbolic computation given a model determining which
    -- branches to take. The calls to fresh are aligned with those in runS so
    -- that the variable names match the second time around.
    relS :: Model -> Fresh a
}

instance Functor Symbolic where
    fmap f x = Symbolic {
        runS = fmap f <$> runS x,
        relS = \m -> f <$> relS x m
    }
    

instance SmtenHS1 Symbolic where
    ite1 p a b = Symbolic {
        runS = do
          ~(pa, va) <- runS a
          ~(pb, vb) <- runS b
          return (ite p pa pb, ite p va vb),

        relS = \m -> do
          va <- relS a m
          vb <- relS b m
          return (ite p va vb)
      }

    unreachable1 = Symbolic {
        runS = return (unreachable, unreachable),
        relS = error "Symbolic.unreachable reached"
    }

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic {
    runS = return (trueF, x),
    relS = \m -> return x
}

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic {
    runS = do
       (px, vx) <- runS x
       (pf, vf) <- runS (f vx)
       return (px `andF` pf, vf),
    relS = \m -> do
       vx <- relS x m
       vf <- relS (f vx) m
       return vf
 }
       

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic {
    runS = return (falseF, unreachable),
    relS = \m -> return (error "Symbolic.relS.mzero reached")
 }

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic {
    runS = do
        p <- varF <$> fresh
        runS $ ite1 p a b,

    relS = \m -> do
        p <- lookupBool m <$> fresh
        relS (ite1 (boolF p) a b) m
 }

free_Integer :: Symbolic IntegerF
free_Integer = Symbolic {
    runS = do
        v <- var_IntegerF <$> fresh
        return (trueF, v),

    relS = \m -> do
        v <- lookupInteger m <$> fresh
        return (integerF v)
 }


free_Bit :: SingI Nat n -> Symbolic (BitF n)
free_Bit w = Symbolic {
    runS = do
        v <- var_BitF (__deNewTyDGSingI w) <$> fresh
        return (trueF, v),

    relS = \m -> do
        v <- lookupBit m (__deNewTyDGSingI w) <$> fresh
        return (bitF v)
 }


run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  case (runFresh $ runS q) of
     (BoolF a b x_, x) -> do
       -- Try to find a solution in 'a', a finite part of the formula.
       ares <- solve s a
       case ares of
         Just m -> return (S.__Just ({-# SCC "Realize" #-} runFresh $ relS q m))
         Nothing -> do
            -- There was no solution found in 'a'.
            -- Check if we have to evaluate b*x_
            bres <- solve s b
            case bres of
               Nothing -> return S.__Nothing
               Just _ -> run_symbolic s (q { runS = return (b *. x_, x)})

