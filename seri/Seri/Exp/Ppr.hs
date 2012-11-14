
{-# LANGUAGE PatternGuards #-}

module Seri.Exp.Ppr () where

import Seri.Ppr
import Seri.Exp.Exp
import Seri.Exp.Sugar

instance Ppr Exp where
    ppr e | Just s <- de_stringE e = text (show s)

    ppr (LitE l) = ppr l
    ppr (ConE s) = ppr s
    ppr (VarE s) = ppr s
    ppr (AppE f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LamE s x) = vcat [
        text "\\" <> ppr s <+> text "->",
        nest tabwidth (ppr x)
      ]
    ppr (CaseE x k y n)
        = text "case" <+> parens (ppr x) <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr k <+> text "->" <+> ppr y,
                    text "_" <+> text "->" <+> ppr n
                 ]) $+$ text "}"

