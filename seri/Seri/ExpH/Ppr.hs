
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Ppr () where

import Seri.Lit
import Seri.Sig
import Seri.Type
import Seri.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.Sugar2

instance Ppr ExpH where
    ppr e | Just v <- de_stringEH e = text (show v)
    ppr e | Just xs <- de_listEH e
      = text "[" <> sep (punctuate comma (map ppr xs)) <> text "]"
    ppr e | Just xs <- de_tupleEH e
      = text "(" <> sep (punctuate comma (map ppr xs)) <> text ")"
    ppr e | Just (s, _, v, f) <- de_letEH e
      = text "let" <+> text "{"
            <+> ppr s <+> text "=" <+> ppr v
            <+> text "}" <+> text "in" <+> ppr (f (VarEH s))

    ppr (LitEH l) = ppr l
    ppr (ConEH _ n _ xs) = ppr (appsEH (varEH (Sig n UnknownT)) xs)
    ppr (VarEH s) = ppr s
    ppr (PrimEH _ n _ _ xs) = ppr (appsEH (varEH (Sig n UnknownT)) xs)
    ppr (AppEH _ f x) = parens (ppr f) <+> parens (ppr x)
    ppr (LamEH _ s _ f) = text "\\" <+> ppr s <+> text "->" <+> text "..."
    ppr (CaseEH _ e1 p e2 e3)
        = text "case" <+> parens (ppr e1) <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr p <+> text "->" <+> ppr e2,
                    text "_" <+> text "->" <+> ppr e3
                  ]) $+$ text "}"
    ppr (ErrorEH t s) = text "error" <+> text s

