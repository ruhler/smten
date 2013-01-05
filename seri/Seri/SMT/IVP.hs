
{-# LANGUAGE PatternGuards #-}

-- Inferred Value Propagation optimization
module Seri.SMT.IVP (ivp) where

import qualified Data.Map as Map

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.ExpH

-- Perform inferred value propagation on the given expression.
-- Assumes the expression may be looked at in its entirety.
--
-- TODO: preserve sharing.
ivp :: ExpH -> ExpH
ivp = ivp' Map.empty

ivp' :: Map.Map Name ExpH -> ExpH -> ExpH
ivp' m e
 | LitEH {} <- e = e
 | ConEH _ n s xs <- e = identify $ \id -> ConEH id n s (map (ivp' m) xs)
 | VarEH (Sig n t) <- e =
     case Map.lookup n m of
        Just v -> v
        Nothing -> e
 | PrimEH _ _ _ f xs <- e = f (map (ivp' m) xs)
 | AppEH _ f x <- e = appEH (ivp' m f) (ivp' m x)
 | LamEH _ s t f <- e = lamEH s t $ \x -> ivp' m (f x)
 | CaseEH _ x k y d <- e =
    case (ivp' m x) of
     x'@(VarEH (Sig nm t)) | t == boolT ->
       let Just kv = de_boolEH (conEH k)
           y' = ivp' (Map.insert nm (boolEH kv) m) y
           d' = ivp' (Map.insert nm (boolEH (not kv)) m) d
       in caseEH x' k y' d'
     x' -> caseEH x' k (ivp' m y) (ivp' m d)
 | ErrorEH {} <- e = e



