
module Seri.Target.Inline (inline) where

import Data.Generics

import Seri.Failable
import Seri.Lambda

-- | Inline all variables in the given expression to the given depth.
inline :: Integer -> Env -> Exp -> Exp
inline 0 _ = id
inline depth env
  = let base :: Exp -> Exp
        base e@(VarE s@(Sig n ct)) =
            case attemptM $ lookupVar env s of
                Just (pt, ve) -> inline (depth-1) env (assign (assignments pt ct) ve)
                Nothing -> e
        base e = e
    in everywhere (mkT base)

