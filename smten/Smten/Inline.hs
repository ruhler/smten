
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
inline :: Env -> [Prim] -> Exp -> ExpH
inline env prims =
  let {-# NOINLINE cache #-}
      cache :: IORef (Map.Map Sig (Maybe ExpH))
      cache = unsafePerformIO (newIORef Map.empty)

      priml :: Sig -> Maybe ExpH 
      priml = lookupPrim prims

      lookupIO :: Sig -> IO (Maybe ExpH)
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

      lookupPure :: Sig -> Maybe ExpH
      lookupPure s = unsafePerformIO (lookupIO s)

      inline' :: [(Name, Type)] -> [(Name, ExpH)] -> Exp -> ExpH
      inline' tm m (LitE _ l) = exph $ LitEH l
      inline' tm m (ConE _ s) = conEH (assign tm s)
      inline' tm m (VarE _ (Sig n _)) | Just v <- lookup n m = v
      inline' tm m (VarE _ s) | Just v <- lookupPure (assign tm s) = v
      inline' tm m (VarE _ s) = exph $ VarEH (assign tm s)
      inline' tm m (AppE _ f x) = appEH (inline' tm m f) (inline' tm m x)
      inline' tm m (LamE _ s@(Sig n _) b) = lamEH (assign tm s) (assign tm $ typeof b) $ \x -> inline' tm ((n, x):m) b
      inline' tm m (CaseE _ x k y n) = caseEH (assign tm $ typeof n) (inline' tm m x) (assign tm k) (inline' tm m y) (inline' tm m n)
  in inline' [] []


