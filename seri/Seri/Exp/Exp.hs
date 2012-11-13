
-- | Pure representation of Seri Expressions.
module Seri.Exp.Exp (
        Exp(..)
    ) where

import Seri.Lit
import Seri.Sig

data Exp = LitE Lit
         | ConE Sig
         | VarE Sig
         | AppE Exp Exp
         | LamE Sig Exp
         | CaseE Exp Sig Exp Exp
            -- ^ case e1 of
            --      k -> e2
            --      _ -> e3
            -- Note: if k is a constructor of type (a -> b -> c -> K),
            -- Then e2 should have type: (a -> b -> c -> V),
            -- And  e1 should have type: V
            --  Where V is the type of the case expression.
    deriving(Show, Eq)

