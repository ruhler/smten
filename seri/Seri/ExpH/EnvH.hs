
-- | An environment for fast lookup of variables in elaboration.
module Seri.ExpH.EnvH (
    EnvH(), mkEnvH, lookupVarH, onEnv,
    ) where

import Debug.Trace

import Control.Monad
import Data.Either
import Data.List(nub)
import qualified Data.Map as Map

import Data.IORef
import System.IO.Unsafe

import Seri.Name
import Seri.Sig
import Seri.Failable
import Seri.Type
import Seri.Dec
import Seri.Ppr
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.ToExpH
import Seri.ExpH.Utils

data EnvH = EnvH {
    eh_env :: Env,
    eh_cache :: IORef (Map.Map Sig (Maybe ExpH))
}

mkEnvH :: Env -> EnvH
mkEnvH e = EnvH e (unsafePerformIO (newIORef Map.empty))

lookupIO :: EnvH -> Sig -> IO (Maybe ExpH)
lookupIO (EnvH e r) s@(Sig n ct) = do
    m <- readIORef r
    case Map.lookup s m of
        Just v -> return v
        Nothing -> do
           let x = attemptM $ do
                     (pt, ve) <- lookupVar e s
                     return $ toExpH (assignments pt ct) [] ve
           writeIORef r (Map.insert s x m)
           return x
                       
lookupVarH :: EnvH -> Sig -> Maybe ExpH
lookupVarH e s = unsafePerformIO (lookupIO e s)
    
onEnv :: (Env -> a) -> EnvH -> a
onEnv f = f . eh_env

