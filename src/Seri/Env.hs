
{-# LANGUAGE FlexibleInstances #-}

module Seri.Env (
    Env(), env, val, mkenv, decls, lookupvar, withenv,
    ) where

import qualified Data.Map as Map

import Seri.IR
import Seri.Ppr

data Env x = Env {
    env :: Map.Map Name Dec,
    val :: x
} deriving (Show, Eq)

mkenv :: [Dec] -> x -> Env x
mkenv ds x = Env (Map.fromList $ map (\d@(ValD n _ _) -> (n, d)) ds) x

decls :: Env x -> [Dec]
decls e = Map.elems (env e)

instance Ppr (Map.Map Name Dec) where
    ppr m = ppr (Map.elems m)

lookupvar :: Name -> Env x -> Maybe Dec
lookupvar x (Env e _) = Map.lookup x e

withenv :: Env a -> b -> Env b
withenv (Env m _) x = Env m x

