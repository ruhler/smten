
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Compiled.Smten.Symbolic0 (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic,
    free_Integer, free_Bit,
    ) where

import Control.Concurrent
import Data.Functor
import Data.Maybe
import Data.Monoid (mappend, mempty)

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Types hiding (Integer)
import qualified Smten.Runtime.Result as S
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
     _pred :: S.Bool,

     -- The free variables declared in this computation:
     _free :: [(FreeID, Type)]
}

instance Functor Result where
    fmap f (Result v p fr) = Result (f v) p fr

data Results a = 
    MZero     -- There are no more results.
  | Return (Result a) (IO (Results a))
        -- ^ This is one result, but there may be more coming if you wait
        -- longer.

guardedwith :: S.Bool -> Results a -> Results a
guardedwith gd v =
  case v of
      MZero -> MZero
      Return (Result a p fr) rest ->
         Return (Result a (gd `andB` p) fr) (guardedwith gd <$> rest)

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

-- The time in milliseconds we wait after getting the first result from a
-- merge for the second result to return before giving up.
waittime :: Int
waittime = 0

instance SmtenHS1 Symbolic where
    -- TODO: this should indicate that it may fail.
    -- That is, running (error) should not return Just error, it should 
    -- return error.
    error1 msg = return_symbolic (error0 msg)

    ite1 S.True a _ = a
    ite1 S.False _ b = b
    ite1 pred a b = Symbolic $ do
      mvar <- newEmptyMVar
      forkIO (runS a >>= (putMVar mvar . Just . (,) pred))
      forkIO (runS b >>= (putMVar mvar . Just . (,) (notB pred)))

      -- Wait for the first branch to finish.
      sa <- fromJust <$> takeMVar mvar

      -- Wait at most a little more time for the other branch
      forkIO (threadDelay waittime >> putMVar mvar Nothing)
      msb <- takeMVar mvar

      -- Deal with what we got
      case (sa, msb) of
        ((_, MZero), Nothing) -> do
            (p, r) <- fromJust <$> takeMVar mvar
            return $ guardedwith p r
        ((_, MZero), Just (_, MZero)) -> return MZero
        ((_, MZero), Just (p, r)) -> return $ guardedwith p r
        ((p, Return (Result va pa fa) resta), Nothing) -> do
            let result = Result va (p `andB` pa) fa
                restb = snd . fromJust <$> takeMVar mvar
                rest = ite0 p (Symbolic resta) (Symbolic restb)
            return $ Return result (runS rest)
        ((p, r), Just (_, MZero)) -> return $ guardedwith p r
        ((p, Return (Result va pa fa) resta), Just (_, Return (Result vb pb fb) restb)) -> do
          let v = ite0 p va vb
              p = iteB p pa pb
              f = fa `mappend` fb
              result = Result v p f
              rest = ite0 p (Symbolic resta) (Symbolic restb)
          return $ Return result (runS rest)

    realize1 m x = realize m <$> x

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic $ finish (Result x S.True mempty)

bind_symbolic :: (SmtenHS0 a, SmtenHS0 b) => Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic $ do
   sx <- runS x
   case sx of
       MZero -> return MZero
       Return (Result v p fr) restx -> do
           sfv <- runS (f v)
           case sfv of
               MZero -> runS $ Symbolic restx `bind_symbolic` f
               Return (Result fv fp ffr) restf -> do
                 let result = Result fv (p `andB` fp) (fr `mappend` ffr)
                     rest = runS $ mplus_symbolic (Symbolic restx `bind_symbolic` f) (Symbolic restf)
                 return $ Return result rest


mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic $ return MZero

-- TODO: should we re-use the ite1 implementation for Symbolic here?
-- That way we don't duplicate so much code. The trouble is, we also fail
-- to take advantage of not caring which branch we return.
mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic $ do
    mvar <- newEmptyMVar
    forkIO $ do
       ra <- runS a
       putMVar mvar (Just ra)

    forkIO $ do
       rb <- runS b
       putMVar mvar (Just rb)

    -- Wait for the first branch to finish.
    sa <- fromJust <$> takeMVar mvar

    -- Wait at most a little more time for the other branch
    forkIO (threadDelay waittime >> putMVar mvar Nothing)
    msb <- takeMVar mvar

    -- Deal with what we got
    case (sa, msb) of
       (MZero, Nothing) -> fromJust <$> takeMVar mvar
       (MZero, Just sb) -> return sb
       (Return result rest, Nothing) -> do
           -- TODO: is there any benefit here if instead of calling mplus,
           -- which creates a new mvar, we try to reuse the mvar we
           -- already have?
           let rest' = mplus_symbolic (Symbolic rest) (Symbolic $ fromJust <$> takeMVar mvar)
           return $ Return result (runS rest')
       (Return {}, Just MZero) -> return sa
       (Return (Result va pa fa) resta, Just (Return (Result vb pb fb) restb)) -> do
         fid <- fresh
         let v = ite0 (S.Bool_Var fid) va vb
             p = pa `andB` pb
             f = (fid, BoolT) : (fa `mappend` fb)
            
             result = Result v p f
             rest = mplus_symbolic (Symbolic resta) (Symbolic restb)
         return $ Return result (runS rest)


free_Integer :: Symbolic (S.Integer)
free_Integer = Symbolic $ do
    fid <- fresh
    finish $ Result (S.Integer_Var fid) S.True [(fid, IntegerT)]

free_Bit :: SingI Nat n -> Symbolic (S.Bit n)
free_Bit w = Symbolic $ do
    fid <- fresh
    finish $ Result (S.Bit_Var fid) S.True [(fid, BitT (__deNewTyDGSingI w))]

run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
  sq <- runS q
  case sq of
    MZero -> return S.Nothing
    Return (Result x p f) rest -> do
      solver <- s
      mapM_ (declVar solver) f
      assert solver p
      res <- check solver
      case res of
        S.Sat -> do
           vals <- mapM (getValue solver) f
           m <- model $ zip (map fst f) vals
           case {-# SCC "DoubleCheck" #-} realize m p of
              S.True -> return ()
              -- TODO: We shouldn't allow us to reach an error.
              -- Instead, treat explicit error like an infinite computation?
              -- Only, ideally, smarter.
              S.Bool_Err msg -> doerr msg
              x -> error $ "SMTEN INTERNAL ERROR: SMT solver lied?"
                     ++ " Got: " ++ show x
           cleanup solver
           return (S.Just ({-# SCC "Realize" #-} realize m x))
        S.Unsat -> do
           cleanup solver
           run_symbolic s (Symbolic rest)

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

