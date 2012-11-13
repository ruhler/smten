
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.ToExpH (toExpH) where

import Seri.Type
import Seri.Sig
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

-- | Translate an Exp to our HOAS ExpH representation
toExpH :: [(Sig, ExpH)] -> Exp -> ExpH
toExpH _ (LitE l) = LitEH l
toExpH _ (ConE s) = ConEH s
toExpH m (VarE s) | Just v <- lookup s m = v
toExpH m (VarE s) = VarEH s
toExpH m (AppE f x) = appEH (toExpH m f) (toExpH m x)
toExpH m (LamE s b) = lamEH s $ \x -> toExpH ((s, x):m) b
toExpH m (CaseE x k y n) = CaseEH ES_None (toExpH m x) k (toExpH m y) (toExpH m n)

