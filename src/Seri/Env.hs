
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
lookupvar :: Env Exp -> Maybe Exp
lookupvar (Env e (VarE _ x _)) =
  let look :: [Dec] -> Name -> Maybe Exp
      look [] _ = Nothing
      look ((ValD nd _ e):ds) n | nd == n = Just e
      look (d:ds) n = look ds n
  in look e x

withenv :: Env a -> b -> Env b
withenv (Env m _) x = Env m x

