
module Seri.Exp.Ppr () where

import Seri.Ppr
import Seri.Exp.Exp
import Seri.Exp.Sugar

instance Ppr Exp where
    ppr (LitE l) = ppr l
    ppr (ConE s) = ppr s
    ppr (VarE s) = ppr s
    ppr (AppE f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LamE s x) = text "\\" <> ppr s <+> text "->" <+> ppr x
    ppr (CaseE x k y n)
        = text "case" <+> parens (ppr x) <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr k <+> text "->" <+> ppr y,
                    text "_" <+> text "->" <+> ppr n
                 ]) $+$ text "}"

