
module Seri.ExpH.Ppr () where

import Seri.Lit
import Seri.Ppr
import Seri.ExpH.ExpH

instance Ppr ExpH where
    ppr (LitEH l) = ppr l
    ppr (ConEH s) = ppr s
    ppr (VarEH s) = ppr s
    ppr (AppEH _ f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LamEH _ s f) = text "\\" <+> ppr s <+> text "-> ..."
    ppr (CaseEH _ e1 p e2 e3)
        = text "case" <+> parens (ppr e1) <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr p <+> text "->" <+> ppr e2,
                    text "_" <+> text "->" <+> ppr e3
                  ]) $+$ text "}"

