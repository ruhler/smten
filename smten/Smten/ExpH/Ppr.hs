
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.Ppr () where

import Smten.Lit
import Smten.Sig
import Smten.Type
import Smten.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar
import Smten.ExpH.Sugar2

instance Ppr Thunk where
    ppr e | Just v <- de_stringEH e = text (show v)
    ppr e | Just xs <- de_listEH e
      = text "[" <> sep (punctuate comma (map ppr xs)) <> text "]"
    ppr e | Just xs <- de_tupleEH e
      = text "(" <> sep (punctuate comma (map ppr xs)) <> text ")"
    ppr e = ppr (force e)

instance Ppr ExpH_ where
    ppr (LitEH l) = ppr l
    ppr (ConEH n _ xs) = ppr (appsEH (varEH (Sig n UnknownT)) xs)
    ppr (VarEH s) = ppr s
    ppr (PrimEH n _ _ xs) = ppr (appsEH (varEH (Sig n UnknownT)) xs)
    ppr (LamEH s _ f) = text "\\" <+> ppr s <+> text "->" <+> text "..."
    ppr (IfEH _ e1 e2 e3)
        = text "if" <+> parens (ppr e1)
            <+> text "then" <+> ppr e2
            <+> text "else" <+> ppr e3
    ppr (ErrorEH t s) = text "error" <+> text s

