
-- | An environment for fast lookup of variables in elaboration.
module Seri.ExpH.EnvH (
    EnvH(), mkEnvH, lookupVarH, onEnv,
    ) where

import Debug.Trace

import Control.Monad
import Data.Either
import Data.List(nub)

import Seri.Name
import Seri.Sig
import qualified Seri.HashTable as HT
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
    eh_l1 :: HT.HashTable Sig ExpH,
    eh_l2 :: HT.HashTable Name (Type -> Maybe ExpH)
}

mkEnvH :: Env -> EnvH
mkEnvH e = 
  let isconcrete :: Type -> Bool
      isconcrete t = null (nvarTs t) && null (varTs t)

      getcvarsD :: Dec -> [Sig]
      getcvarsD (ValD (TopSig n _ t) e) =
        let evs = getcvarsE [] e
        in if isconcrete t
             then (Sig n t) : evs
             else evs
      getcvarsD (InstD _ _ ms) = concat [getcvarsE [] e | Method _ e <- ms]
      getcvarsD _ = []

      getcvarsE :: [Name] -> Exp -> [Sig]
      getcvarsE _ (LitE {}) = []
      getcvarsE _ (ConE {}) = []
      getcvarsE bnd (VarE s@(Sig n t)) | n `notElem` bnd && isconcrete t = [s]
      getcvarsE _ (VarE {}) = []
      getcvarsE bnd (AppE a b) = getcvarsE bnd a ++ getcvarsE bnd b
      getcvarsE bnd (LamE (Sig n _) x) = getcvarsE (n:bnd) x
      getcvarsE bnd (CaseE x _ y n) = concat [
            getcvarsE bnd x, getcvarsE bnd y, getcvarsE bnd n]

      getcelem :: Sig -> [(Sig, ExpH)]
      getcelem s@(Sig n ct) = attemptM $ do
        (pt, ve) <- lookupVar e s
        return $ (s, toExpH (assignments pt ct) [] ve)

      getelem :: Dec -> [Either (Sig, ExpH) (Name, Type -> Maybe ExpH)]
      getelem (ValD (TopSig n _ pt) e) | isconcrete pt = [Left (Sig n pt, toExpH [] [] e)]
      getelem (ValD (TopSig n _ pt) e) =
        let f ct = return $ toExpH (assignments pt ct) [] e
        in [Right (n, f)]
      getelem (ClassD _ _ tss) = [Right (n, mkf n) | TopSig n _ _ <- tss]
      getelem _ = []

      elems = concatMap getelem (getDecls e)

      mkf :: Name -> Type -> Maybe ExpH
      mkf n ct = do
        (pt, ve) <- attemptM $ lookupVar e (Sig n ct)
        return $ toExpH (assignments pt ct) [] ve

      --allcvars = nub $ concatMap getcvarsD (getDecls e)
      allcvars = []
      specialize = []
--        Sig (name "Prelude.++") (arrowsT [stringT, stringT, stringT]),
--        Sig (name "Prelude.concat") (arrowsT [listT stringT, stringT]),
--        Sig (name "Prelude.curry")
--            (arrowsT [arrowsT [tupleT [charT, charT], charT],
--                      charT, charT, charT])
--         ]

      l1 = HT.table (lefts elems ++ concatMap getcelem (specialize ++ allcvars))
      l2 = HT.table (rights elems)
  in EnvH e l1 l2

lookupVarH :: EnvH -> Sig -> Maybe ExpH
lookupVarH e s@(Sig n ct) = mplus (HT.lookup s (eh_l1 e)) $ do
    f <- HT.lookup n (eh_l2 e)
    f ct
    
onEnv :: (Env -> a) -> EnvH -> a
onEnv f = f . eh_env

