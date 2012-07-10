
module Seri.Lambda.Utils (
    free
    ) where

import Data.List(nub)

import Seri.Lambda.IR
import Seri.Lambda.Types


-- | Return a list of the free variables in the given expression.
free :: Exp -> [Sig]
free =
  let free' :: [Name] -> Exp -> [Sig]
      free' _ (IntegerE {}) = []
      free' bound (CaseE e ms) = 
        let freem :: Match -> [Sig]
            freem (Match p b) = free' (map (\(Sig n _) -> n) (bindingsP p) ++ bound) b
        in nub $ concat (free' bound e : map freem ms)
      free' bound (AppE a b) = free' bound a ++ free' bound b
      free' bound (LamE (Sig n _) b) = free' (n:bound) b
      free' bound (ConE {}) = []
      free' bound (VarE (Sig n _)) | n `elem` bound = []
      free' bound (VarE s) = [s]
  in free' []

