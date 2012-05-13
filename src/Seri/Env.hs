
{-# LANGUAGE FlexibleInstances #-}

module Seri.Env (
    Env(), val, mkenv, decls, lookupvar, withenv,
    ) where

import Data.Maybe(listToMaybe)
import Data.List(nub)

import Seri.IR
import Seri.Ppr

data Env x = Env {
    env :: [Dec],
    val :: x
} deriving (Show, Eq)

mkenv :: [Dec] -> x -> Env x
mkenv ds x = Env ds x

decls :: Env x -> [Dec]
decls x = env x

-- Look up a ValD with given Name in the given Environment.
lookupValD :: Env a -> Name -> Maybe Dec
lookupValD (Env decls _) n =
  let theValD :: Dec -> Bool
      theValD (ValD nm _ _) = n == nm
      theValD _ = False
  in listToMaybe $ filter theValD decls

-- Look up a DataD with given Name in the given Environment.
lookupDataD :: Env a -> Name -> Maybe Dec
lookupDataD (Env decls _) n =
  let theDataD :: Dec -> Bool
      theDataD (DataD nm _ _) = n == nm
      theDataD _ = False
  in listToMaybe $ filter theDataD decls

-- Look up a ClassD with given Name in the given Environment.
lookupClassD :: Env a -> Name -> Maybe Dec
lookupClassD (Env decls _) n =
  let theClassD :: Dec -> Bool
      theClassD (ClassD nm _ _) = n == nm
      theClassD _ = False
  in listToMaybe $ filter theClassD decls

-- Look up an InstD in the given Environment.
lookupInstD :: Env a -> Name -> [Type] -> Maybe Dec
lookupInstD (Env decls _) n t =
  let theInstD :: Dec -> Bool
      theInstD (InstD nm ts _) = n == nm && t == ts
      theInstD _ = False
  in listToMaybe $ filter theInstD decls

-- Given a VarE in an environment return the value of that variable as
-- determined by the environment.
lookupvar :: Env Exp -> Maybe (Type, Exp)
lookupvar e@(Env _ (VarE _ n Declared)) = do
  (ValD _ t v) <- lookupValD e n
  return (t, v)
lookupvar e@(Env _ (VarE _ x (Instance n ts))) =
  let mlook :: [Method] -> Maybe (Type, Exp)
      mlook [] = Nothing
      mlook ((Method nm e):ms) | nm == x = Just (error "TODO: look up type of method in ClassD", e)
      mlook (m:ms) = mlook ms
  in do
      InstD _ _ ms <- lookupInstD e n ts  
      mlook ms

withenv :: Env a -> b -> Env b
withenv (Env m _) x = Env m x


---- Return the set of declarations a given expression depends on.
--declarations :: Env Exp -> [Dec]
--declarations (Env m e) =
--  let query :: Exp -> [Dec]
--      query e = []
--
--  in (everything (\a b -> nub $ a ++ b) $ mkQ [] query) e

