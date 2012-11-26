
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.ToExpH (toExpH) where

import Seri.Type
import Seri.Sig
import Seri.Name
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

-- | Translate an Exp to our HOAS ExpH representation
-- Simultaneously converts types.
toExpH :: [(Name, Type)] -> [(Sig, ExpH)] -> Exp -> ExpH
toExpH _ _ (LitE l) = LitEH l
toExpH tm _ (ConE s) = ConEH (assign tm s)
toExpH _ m (VarE s) | Just v <- lookup s m = v
toExpH tm m (VarE s) = VarEH (assign tm s)
toExpH tm m (AppE f x) = appEH (toExpH tm m f) (toExpH tm m x)
toExpH tm m (LamE s b) = lamEH (assign tm s) $ \x -> toExpH tm ((s, x):m) b
toExpH tm m (CaseE x k y n) = CaseEH ES_None (toExpH tm m x) (assign tm k) (toExpH tm m y) (toExpH tm m n)

