
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
import Smten.Runtime.RunBoth
import Smten.Runtime.Types hiding (Integer)
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver

import Smten.Compiled.Smten.Data.Bit0 as S
import qualified Smten.Compiled.Smten.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits
import qualified Smten.Compiled.Smten.Smten.Integer as S
import qualified Smten.Runtime.Types as S

-- The result of a symbolic computation.
data Result a = Result {
     -- The result of this computation:
     _value :: a,

     -- The condition required for this computation:
     _pred :: S.Bool
}

instance Functor Result where
    fmap f (Result v p) = Result (f v) p

data Results a = 
    MZero     -- There are no more results.
  | Return (Result a) (IO (Results a))
        -- ^ This is one result, but there may be more coming if you wait
        -- longer.

guardedwith :: S.Bool -> Results a -> Results a
guardedwith gd v =
  case v of
      MZero -> MZero
      Return (Result a p) rest ->
         Return (Result a (gd `andF` p)) (guardedwith gd <$> rest)

finish :: Result a -> IO (Results a)
finish r = return (Return r (return MZero))

instance Functor Results where
    fmap f MZero = MZero
    fmap f (Return this rest) = Return (f <$> this) (fmap f <$> rest)
     
newtype Symbolic a = Symbolic {
    runS :: IO (Results a)
}

instance Functor Symbolic where
    fmap f x = Symbolic $ fmap f <$> runS x

instance SmtenHS1 Symbolic where
    error1 msg = doerr msg

    ite1 S.True a _ = a
    ite1 S.False _ b = b
    ite1 pred a b = Symbolic $ do
      runresult <- runBoth ((,) pred <$> runS a) ((,) (notF pred) <$> runS b)
      case runresult of
        OneFinished (_, MZero) rest2 -> do
            (p, r) <- rest2
            return $ guardedwith p r
        OneFinished (p, Return (Result v1 p1) rest1) rest2 -> do
            let result = Result v1 (p `andF` p1)
                rest = ite0 p (Symbolic rest1) (Symbolic $ snd <$> rest2)
            return $ Return result (runS rest)
        BothFinished (_, MZero) (p, r) -> return $ guardedwith p r
        BothFinished (p, r) (_, MZero) -> return $ guardedwith p r
        BothFinished (p, Return (Result v1 p1) rest1)
                     (_, Return (Result v2 p2) rest2) -> do
          let result = Result (ite0 p v1 v2) (iteF p p1 p2)
              rest = ite0 p (Symbolic rest1) (Symbolic rest2)
          return $ Return result (runS rest)

    realize1 m x = realize m <$> x

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic $ finish (Result x S.True)

bind_symbolic :: (SmtenHS0 a, SmtenHS0 b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic $ do
   sx <- runS x
   case sx of
       MZero -> return MZero
       Return (Result v p) restx -> do
          runS $ mplus_symbolic (Symbolic restx `bind_symbolic` f) (Symbolic $ guardedwith p <$> runS (f v))


mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic $ return MZero

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic $ do
    runresult <- runBoth (runS a) (runS b) 
    case runresult of
       OneFinished MZero rest2 -> rest2
       OneFinished (Return result rest1) rest2 -> do
           let rest = mplus_symbolic (Symbolic rest1) (Symbolic rest2)
           return $ Return result (runS rest)
       BothFinished MZero result -> return result
       BothFinished result MZero -> return result
       BothFinished (Return (Result v1 p1) rest1)
                    (Return (Result v2 p2) rest2) -> do
         which <- S.Bool_Var <$> fresh
         let result = Result (ite0 which v1 v2) (ite0 which p1 p2)
             rest = mplus_symbolic (Symbolic rest1) (Symbolic rest2)
         return $ Return result (runS rest)


free_Integer :: Symbolic (S.Integer)
free_Integer = Symbolic $ do
    fid <- fresh
    finish $ Result (S.Integer_Var fid) S.True

free_Bit :: SingI Nat n -> Symbolic (S.Bit n)
free_Bit w = Symbolic $ do
    fid <- fresh
    finish $ Result (S.Bit_Var (__deNewTyDGSingI w) fid) S.True

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  sq <- runS q
  case sq of
    MZero -> return S.__Nothing
    Return (Result x p) rest -> do
      res <- solve s p
      case res of
        P.Just m -> do
           case {-# SCC "DoubleCheck" #-} realize m p of
              S.True -> return ()
              -- TODO: We shouldn't allow us to reach an error.
              -- Instead, treat explicit error like an infinite computation?
              -- Only, ideally, smarter.
              S.Bool_Err msg -> doerr msg
              x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied?"
                     ++ " Got: " ++ show x
           return (S.__Just ({-# SCC "Realize" #-} realize m x))
        P.Nothing -> run_symbolic s (Symbolic rest)
