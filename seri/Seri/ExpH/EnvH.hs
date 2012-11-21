
-- | An environment for fast lookup of variables in elaboration.
module Seri.ExpH.EnvH (
    EnvH(), mkEnvH, lookupVarH, onEnv,
    ) where

import Debug.Trace

import Seri.Name
import Seri.Sig
import qualified Seri.HashTable as HT
import Seri.Failable
import Seri.Type
import Seri.Dec
import Seri.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.ToExpH

data EnvH = EnvH Env (HT.HashTable Name (Type -> Maybe ExpH))

mkEnvH :: Env -> EnvH
mkEnvH e = 
  let getelem :: Dec -> [(Name, Type -> Maybe ExpH)]
      getelem (ValD (TopSig n _ pt) e) | null (nvarTs pt) && null (varTs pt)
        = [(n, \_ -> return (toExpH [] e))]
      getelem (ValD (TopSig n _ pt) e) =
        let f :: Type -> Maybe ExpH
            f ct = return (toExpH [] $ assign (assignments pt ct) e)
        in [(n, f)]
      getelem (ClassD _ _ tss) = [(n, mkf n) | TopSig n _ _ <- tss]
      getelem _ = []

      elems = concatMap getelem (getDecls e)

      mkf :: Name -> Type -> Maybe ExpH
      mkf n ct = do
        (pt, ve) <- attemptM $ lookupVar e (Sig n ct)
        return $ toExpH [] $ assign (assignments pt ct) ve

      m = HT.table elems
  in EnvH e m

lookupVarH :: EnvH -> Sig -> Maybe ExpH
lookupVarH (EnvH _ m) (Sig n ct) = do
    f <- HT.lookup n m
    f ct
    
onEnv :: (Env -> a) -> EnvH -> a
onEnv f (EnvH e _) = f e

