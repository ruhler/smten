
{-# LANGUAGE PatternGuards #-}

module Smten.Inline (inline) where


import System.IO.Unsafe

import Control.Monad
import Data.IORef
import qualified Data.Map as Map

import Smten.Type
import Smten.Sig
import Smten.Failable
import Smten.Name
import Smten.Exp
import Smten.Dec
import Smten.ExpH
import Smten.Prim

-- | Inline all variables from environment into the given expression.
inline :: Env -> [Prim] -> Exp -> Thunk
inline env prims =
  let {-# NOINLINE cache #-}
      cache :: IORef (Map.Map Sig (Maybe Thunk))
      cache = unsafePerformIO (newIORef Map.empty)

      priml :: Sig -> Maybe Thunk 
      priml = lookupPrim prims

      lookupIO :: Sig -> IO (Maybe Thunk)
      lookupIO s@(Sig n ct) = do
         m <- readIORef cache
         case Map.lookup s m of
            Just v -> return v
            Nothing -> do
              let inprims = priml s
                  inenv = attemptM $ do
                         (pt, ve) <- lookupVar env s
                         return $ inline' (assignments pt ct) [] ve
                  x = mplus inprims inenv
                
              modifyIORef cache (Map.insert s x)
              return x

      lookupPure :: Sig -> Maybe Thunk
      lookupPure s = unsafePerformIO (lookupIO s)

      inline' :: [(Name, Type)] -> [(Sig, Thunk)] -> Exp -> Thunk
      inline' tm m (LitE l) = thunkNS $ LitEH l
      inline' tm m (ConE s) = conEH (assign tm s)
      inline' tm m (VarE s) | Just v <- lookup s m = v
      inline' tm m (VarE s) | Just v <- lookupPure (assign tm s) = v
      inline' tm m (VarE s) = thunkNS $ VarEH (assign tm s)
      inline' tm m (AppE f x) = appEH (inline' tm m f) (inline' tm m x)
      inline' tm m (LamE s b) = lamEH (assign tm s) (assign tm $ typeof b) $ \x -> inline' tm ((s, x):m) b
      inline' tm m (CaseE x k y n) = caseEH (typeof y) (inline' tm m x) (assign tm k) (inline' tm m y) (inline' tm m n)
  in inline' [] []


