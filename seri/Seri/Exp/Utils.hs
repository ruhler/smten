
module Seri.Exp.Utils (free, free') where

import Data.List(nub)

import Seri.Name
import Seri.Sig
import Seri.Exp.Exp

-- | Return a list of the free variables in the given expression.
free :: Exp -> [Sig]
free =
  let free' :: [Name] -> Exp -> [Sig]
      free' _ (LitE {}) = []
      free' _ (ConE {}) = []
      free' bound (VarE (Sig n _)) | n `elem` bound = []
      free' _ (VarE s) = [s]
      free' bound (AppE a b) = free' bound a ++ free' bound b
      free' bound (LamE (Sig n _) b) = free' (n:bound) b
      free' bound (CaseE x k y n) = free' bound x ++ free' bound y ++ free' bound n
  in nub . free' []

free' :: Exp -> [Name]
free' e = [n | Sig n _ <- free e]

