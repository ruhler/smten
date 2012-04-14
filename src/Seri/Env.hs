
{-# LANGUAGE FlexibleInstances #-}

module Seri.Env (
    Env(), env, val,
    withenv, putenv, lookupvar,
    ) where

import qualified Data.Map as Map

import Seri.IR
import Seri.Ppr

data Env x = Env {
    env :: Map.Map Name Dec,
    val :: x
} deriving (Show, Eq)

instance Monad Env where
    return x = Env Map.empty x
    (>>=) (Env ad a) f =
        let (Env bd b) = f a
        in Env (Map.union ad bd) b

instance Ppr (Map.Map Name Dec) where
    ppr m = ppr (Map.elems m)

withenv :: Env a -> b -> Env b
withenv e x = e >> return x

putenv :: Dec -> Env a -> Env a
putenv d@(ValD n _ _) (Env e x) = Env (Map.insert n d e) x

lookupvar :: Name -> Env x -> Maybe Dec
lookupvar x (Env e _) = Map.lookup x e

