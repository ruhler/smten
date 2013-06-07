
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}

module Smten.Dec.Utils () where

import Data.List(nub)

import Smten.Name
import Smten.Type
import Smten.Dec.Dec

instance Assign Class where
    assignl f (Class nm ts) = Class nm (assignl f ts)

instance AssignK Class where
    assignkl f (Class nm ts) = Class nm (assignkl f ts)

instance AssignK TopSig where
    assignkl f (TopSig n ctx ty) = TopSig n (assignkl f ctx) (assignkl f ty)

instance AssignK TyVar where
    assignkl f (TyVar n k) = TyVar n (assignkl f k)

instance AssignK Con where
    assignkl f (Con n ts) = Con n (assignkl f ts)

instance AssignK a => AssignK [a] where
    assignkl f = map (assignkl f)

instance AssignK TopExp where
    assignkl f (TopExp t e) = TopExp (assignkl f t) e

instance AssignK Dec where
    assignkl f d
      | ValD l e <- d = ValD l (assignkl f e)
      | DataD l n vs cs <- d = DataD l n (assignkl f vs) (assignkl f cs)
      | ClassD l ctx n vs ts <- d = ClassD l (assignkl f ctx) n (assignkl f vs) (assignkl f ts)
      | InstD l ctx cls ms <- d = InstD l (assignkl f ctx) (assignkl f cls) ms
      | PrimD l n t <- d = PrimD l n (assignkl f t)
      | AsInHaskellD {} <- d = d

instance VarTs Class where
    varTs (Class nm ts) = nub (concatMap varTs ts)

instance VarTs TyVar where
    varTs (TyVar n k) = [(n, k)]

instance VarTs TopSig where
    varTs (TopSig _ _ t) = varTs t

instance VarTs (Name, Kind) where
    varTs x = [x]

    
