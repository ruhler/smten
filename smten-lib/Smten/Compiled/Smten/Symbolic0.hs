
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
import Smten.Runtime.Types hiding (Integer)
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver

import Smten.Compiled.Smten.Data.Bit0 as S
import qualified Smten.Compiled.Smten.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits
import qualified Smten.Compiled.Smten.Smten.Integer as S
import qualified Smten.Runtime.Types as S

newtype Symbolic a = Symbolic {
    runS :: Fresh (S.Bool, a)
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
return_symbolic x = Symbolic $ return (S.True, x)

bind_symbolic :: (SmtenHS0 a, SmtenHS0 b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic $ do
   ~(px, vx) <- runS x
   ~(pf, vf) <- runS (f vx)
   return (px `andF` pf, vf)

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic $ return (S.False, error "mzero")

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic $ do
    p <- S.Bool_Var <$> fresh
    runS $ ite1 p a b

free_Integer :: Symbolic (S.Integer)
free_Integer = Symbolic $ do
    v <- S.Integer_Var <$> fresh
    return (S.True, v)

free_Bit :: SingI Nat n -> Symbolic (S.Bit n)
free_Bit w = Symbolic $ do
    v <- S.Bit_Var (__deNewTyDGSingI w) <$> fresh
    return (S.True, v)

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  let (p, x) = runFresh $ runS q
      (pfinite, mpbig) = partial p
  res <- solve s pfinite
  case res of
    P.Just m -> do
       case {-# SCC "DoubleCheck" #-} realize m p of
          S.True -> return ()
          x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied?"
                 ++ " Got: " ++ show x
       return (S.__Just ({-# SCC "Realize" #-} realize m x))
    P.Nothing -> case mpbig of
                    P.Nothing -> return S.__Nothing
                    P.Just pbig -> run_symbolic s (Symbolic $ return (pbig, x))

