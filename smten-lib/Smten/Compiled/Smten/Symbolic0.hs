
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic0 (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Data.Functor
import Data.Monoid (mappend, mempty)

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Types hiding (Integer)
import Smten.Runtime.Result
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver

import Smten.Compiled.Smten.Data.Bit0 as S
import Smten.Compiled.Smten.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits
import qualified Smten.Compiled.Smten.Smten.Integer as S
import qualified Smten.Runtime.Types as S

data Sym a =
   MZero
 | Return {
     -- The result of this computation:
     _value :: a,

     -- The condition required for this computation:
     _pred :: S.Bool,

     -- The free variables declared in this computation:
     _free :: [(FreeID, Type)]
}
     
newtype Symbolic a = Symbolic {
    -- Run a symbolic computation.
    -- Note: This is in the IO monad in order to allocate globally fresh
    -- variable names. We could probably switch to State or ST instead.
    runS :: IO (Sym a)
}

instance Functor Symbolic where
    fmap f x = Symbolic $ do
       sx <- runS x
       case sx of
         MZero -> return MZero
         Return v p fr -> return $ Return (f v) p fr

instance Monad Symbolic where
    return x = Symbolic . return $ Return x S.True mempty

    (>>=) x f = Symbolic $ do
        sx <- runS x
        case sx of
            MZero -> return MZero
            Return v p fr -> do
                sfv <- runS (f v)
                case sfv of
                    MZero -> return MZero
                    Return fv fp ffr -> do
                      return $ Return fv (p `andB` fp) (fr `mappend` ffr)

instance SmtenHS1 Symbolic where
    -- TODO: this should indicate that it may fail.
    -- That is, running (error) should not return Just error, it should 
    -- return error.
    error1 msg = return (error0 msg)

    ite1 S.True a _ = a
    ite1 S.False _ b = b
    ite1 p a b = Symbolic $ do
      sa <- runS a
      sb <- runS b
      case (sa, sb) of
        (MZero, MZero) -> return MZero
        (MZero, Return vb pb fb) -> return $ Return vb (notB p `andB` pb) fb 
        (Return va pa fa, MZero) -> return $ Return va (p `andB` pa) fa
        (Return va pa fa, Return vb pb fb) -> do
          let v = ite0 p va vb
              p = iteB p pa pb
              f = fa `mappend` fb
          return $ Return v p f

    realize1 m x = realize m <$> x

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic $ return MZero

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic $ do
    sa <- runS a
    sb <- runS b
    case (sa, sb) of
       (MZero, _) -> return sb
       (_, MZero) -> return sa
       (Return va pa fa, Return vb pb fb) -> do
         fid <- fresh
         let v = ite0 (S.Bool_Var fid) va vb
             p = pa `andB` pb
             f = (fid, BoolT) : (fa `mappend` fb)
         return $ Return v p f

free_Integer :: Symbolic (S.Integer)
free_Integer = Symbolic $ do
    fid <- fresh
    return $ Return (S.Integer_Var fid) S.True [(fid, IntegerT)]

free_Bit :: SingI Nat n -> Symbolic (S.Bit n)
free_Bit w = Symbolic $ do
    fid <- fresh
    return $ Return (S.Bit_Var fid) S.True [(fid, BitT (__deNewTyDGSingI w))]

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  sq <- runS q
  case sq of
    MZero -> return S.Nothing
    Return x p f -> do
      solver <- s
      mapM_ (declVar solver) f
      assert solver p
      res <- check solver
      case res of
        Sat -> do
           vals <- mapM (getValue solver) f
           m <- model $ zip (map fst f) vals
           case {-# SCC "DoubleCheck" #-} realize m p of
              S.True -> return ()
              S.Bool_Err msg -> doerr msg
              x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied?"
                     ++ " Got: " ++ show x
           cleanup solver
           return (S.Just ({-# SCC "Realize" #-} realize m x))
        Unsat -> do
           cleanup solver
           return S.Nothing

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

