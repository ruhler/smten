
module Seri.Target.Inline (inline) where

import Seri.Failable
import Seri.Lambda

-- | Inline all variables in the given expression to the given depth.
inline :: Integer -> Env -> Exp -> Exp
inline = inline' []

-- | Same as inline, but don't inline any variables in the given bound set.
inline' :: [Name] -> Integer -> Env -> Exp -> Exp
inline' _ 0 _ e = e
inline' bound depth env e =
  let inme = inline' bound depth env
  in case e of
        LitE {} -> e
        CaseE a ms ->
           let imatch (Match p b) =
                let nbound = bindingsP' p
                in Match p (inline' (nbound ++ bound) depth env b)
           in CaseE (inme a) (map imatch ms)
        AppE a b -> AppE (inme a) (inme b)
        LamE (Sig n t) b -> LamE (Sig n t) (inline' (n:bound) depth env b)
        ConE {} -> e
        (VarE s@(Sig n ct)) | not (n `elem` bound) ->
            case attemptM $ lookupVar env s of
                Just (pt, ve) -> inline' bound (depth-1) env (assign (assignments pt ct) ve)
                Nothing -> e
        VarE {} -> e

