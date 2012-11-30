
{-# LANGUAGE PatternGuards #-}

module Seri.Inline (inline) where


import System.IO.Unsafe

import Data.IORef
import qualified Data.Map as Map

import Seri.Type
import Seri.Sig
import Seri.Failable
import Seri.Name
import Seri.Exp
import Seri.Dec
import Seri.ExpH

-- | Inline all variables from environment into the given expression.
inline :: Env -> Exp -> ExpH
inline env =
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
                         return $ inline' (assignments pt ct) [] ve
              writeIORef cache (Map.insert s x m)
              return x

      lookupPure :: Sig -> Maybe ExpH
      lookupPure s = unsafePerformIO (lookupIO s)

      inline' :: [(Name, Type)] -> [(Sig, ExpH)] -> Exp -> ExpH
      inline' tm m (LitE l) = LitEH l
      inline' tm m (ConE s) = ConEH (assign tm s)
      inline' tm m (VarE s) | Just v <- lookup s m = v
      inline' tm m (VarE s) | Just v <- lookupPure (assign tm s) = v
      inline' tm m (VarE s) = VarEH (assign tm s)
      inline' tm m (AppE f x) = appEH (inline' tm m f) (inline' tm m x)
      inline' tm m (LamE s b) = lamEH (assign tm s) $ \x -> inline' tm ((s, x):m) b
      inline' tm m (CaseE x k y n) = CaseEH ES_None (inline' tm m x) (assign tm k) (inline' tm m y) (inline' tm m n)
  in inline' [] []


