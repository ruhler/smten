
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.ToExpH (toExpH) where


import System.IO.Unsafe

import Data.IORef
import qualified Data.Map as Map

import Seri.Type
import Seri.Sig
import Seri.Failable
import Seri.Name
import Seri.Exp
import Seri.Dec
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

toExpH :: Env -> Exp -> ExpH
toExpH env =
  let {-# NOINLINE cache #-}
      cache :: IORef (Map.Map Sig (Maybe ExpH))
      cache = unsafePerformIO (newIORef Map.empty)

      lookupIO :: Sig -> IO (Maybe ExpH)
      lookupIO s@(Sig n ct) = do
         m <- readIORef cache
         case Map.lookup s m of
            Just v -> return v
            Nothing -> do
              let x = attemptM $ do
                         (pt, ve) <- lookupVar env s
                         return $ toExpH' (assignments pt ct) [] ve
              writeIORef cache (Map.insert s x m)
              return x

      lookupPure :: Sig -> Maybe ExpH
      lookupPure s = unsafePerformIO (lookupIO s)

      toExpH' :: [(Name, Type)] -> [(Sig, ExpH)] -> Exp -> ExpH
      toExpH' tm m (LitE l) = LitEH l
      toExpH' tm m (ConE s) = ConEH (assign tm s)
      toExpH' tm m (VarE s) | Just v <- lookup s m = v
      toExpH' tm m (VarE s) | Just v <- lookupPure (assign tm s) = v
      toExpH' tm m (VarE s) = VarEH (assign tm s)
      toExpH' tm m (AppE f x) = appEH (toExpH' tm m f) (toExpH' tm m x)
      toExpH' tm m (LamE s b) = lamEH (assign tm s) $ \x -> toExpH' tm ((s, x):m) b
      toExpH' tm m (CaseE x k y n) = CaseEH ES_None (toExpH' tm m x) (assign tm k) (toExpH' tm m y) (toExpH' tm m n)
  in toExpH' [] []


