
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

import qualified Smten.Compiled.Smten.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits

newtype Symbolic a = Symbolic {
    runS :: Fresh (BoolF, a)
}

instance Functor Symbolic where
    fmap f x = Symbolic $ fmap f <$> runS x

instance SmtenHS1 Symbolic where
    ite1 p a b = Symbolic $ do
      ~(pa, va) <- runS a
      ~(pb, vb) <- runS b
      return (ite p pa pb, ite p va vb)

    realize1 m x = Symbolic $ do
        (p, v) <- runS x
        return (realize m p, realize m v)

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic $ return (trueF, x)

bind_symbolic :: (SmtenHS0 a, SmtenHS0 b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic $ do
   ~(px, vx) <- runS x
   ~(pf, vf) <- runS (f vx)
   return (px `andF` pf, vf)

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic $ return (falseF, error "mzero")

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic $ do
    p <- varF <$> fresh
    runS $ ite1 p a b

free_Integer :: Symbolic IntegerF
free_Integer = Symbolic $ do
    v <- var_IntegerF <$> fresh
    return (trueF, v)

free_Bit :: SingI Nat n -> Symbolic (BitF n)
free_Bit w = Symbolic $ do
    v <- var_BitF (__deNewTyDGSingI w) <$> fresh
    return (trueF, v)

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  case (runFresh $ runS q) of
     (BoolF a b x_, x) -> do
       -- Try to find a solution in 'a', a finite part of the formula.
       ares <- solve s a
       case ares of
         Just m -> return (S.__Just ({-# SCC "Realize" #-} realize m x))
         Nothing -> do
            -- There was no solution found in 'a'.
            -- Check if we have to evaluate b*x_
            bres <- solve s b
            case bres of
               Nothing -> return S.__Nothing
               Just _ -> run_symbolic s (Symbolic $ return (b *. x_, x))

