
{-# LANGUAGE FlexibleInstances #-}

module Seri.Env (
    Env(), val, mkenv, decls, lookupvar, withenv,
    ) where

import qualified Data.Map as Map

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

-- Given a VarE in an environment return the value of that variable as
-- determined by the environment.
lookupvar :: Env Exp -> Maybe (Type, Exp)
lookupvar (Env e (VarE _ x NoInst)) =
  let look :: [Dec] -> Name -> Maybe (Type, Exp)
      look [] _ = Nothing
      look ((ValD nd t e):ds) n | nd == n = Just (t, e)
      look (d:ds) n = look ds n
  in look e x
lookupvar (Env e (VarE _ x (Inst n ts))) =
  let mlook :: [Method] -> Maybe (Type, Exp)
      mlook [] = Nothing
      mlook ((Method nm e):ms) | nm == x = Just (error "TODO: look up type of method in ClassD", e)
      mlook (m:ms) = mlook ms

      look :: [Dec] -> Maybe (Type, Exp)
      look [] = Nothing
      look ((InstD ni tsi  meths):ds) | (n == ni && ts == tsi) = mlook meths
      look (d:ds) = look ds
  in look e

withenv :: Env a -> b -> Env b
withenv (Env m _) x = Env m x

