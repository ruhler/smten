
{-# LANGUAGE PatternGuards #-}

module Smten.Typing.ConTs (ConTs(..)) where

import qualified Data.Set as Set

import Smten.Type
import Smten.Name
import Smten.Dec

class ConTs a where
    conTs :: a -> Set.Set Name

instance ConTs Type where
    conTs t
       | ConT n _ <- t = Set.singleton n
       | AppT a b <- t = conTs a `Set.union` conTs b
       | otherwise = Set.empty

instance ConTs TopExp where
    conTs (TopExp t _) = conTs t

instance ConTs Dec where
    conTs d
      | ValD _ e <- d = conTs e
      | DataD _ n _ cs <- d= Set.unions [conTs cs, Set.singleton n]
      | ClassD _ ctx n _ ts <- d 
            = Set.unions $ [conTs ctx, Set.singleton n, conTs ts]
      | InstD _ ctx cls _ <- d = Set.unions [conTs ctx, conTs cls]
      | PrimD _ _ t <- d = conTs t
      | AsInHaskellD {} <- d = Set.empty

instance ConTs Con where
    conTs (Con _ ts) = conTs ts

instance (ConTs a) => ConTs [a] where
    conTs xs = Set.unions (map conTs xs)

instance ConTs Env where
    conTs e = conTs (getDecls e)

instance ConTs TopSig where
    conTs (TopSig _ ctx ty) = Set.unions [conTs ctx, conTs ty]

instance ConTs Class where
    conTs (Class n ts) = Set.unions [Set.singleton n, conTs ts]

