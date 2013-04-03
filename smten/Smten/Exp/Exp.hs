
-- | Pure representation of Smten Expressions.
module Smten.Exp.Exp (
        Exp(..)
    ) where

import Smten.Lit
import Smten.Location
import Smten.Sig

data Exp = LitE Location Lit
         | ConE Location Sig
         | VarE Location Sig
         | AppE Location Exp Exp
         | LamE Location Sig Exp
         | CaseE Location Exp Sig Exp Exp
            -- ^ case e1 of
            --      k -> e2
            --      _ -> e3
            -- Note: if k is a constructor of type (a -> b -> c -> K),
            -- Then e2 should have type: (a -> b -> c -> V),
            -- And  e1 should have type: V
            --  Where V is the type of the case expression.
    deriving(Show, Eq)

instance Locate Exp where
    locate (LitE l _) = l
    locate (ConE l _) = l
    locate (VarE l _) = l
    locate (AppE l _ _) = l
    locate (LamE l _ _) = l
    locate (CaseE l _ _ _ _) = l

