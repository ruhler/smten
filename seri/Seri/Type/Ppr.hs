
{-# LANGUAGE PatternGuards #-}

module Seri.Type.Ppr () where

import Seri.Ppr
import Seri.Type.Type
import Seri.Type.Sugar

instance Ppr NType where
    ppr (ConNT i) = integer i
    ppr (VarNT v) = ppr v
    ppr (AppNT o a b) = parens (ppr a <+> text o <+> ppr b)

instance Ppr Type where
    -- Special case for list type
    ppr t | Just v <- de_listT t = text "[" <> ppr v <> text "]"

    ppr (ConT n) = ppr n
    ppr (AppT a b) = parens (ppr a) <+> parens (ppr b)
    ppr (VarT n) = ppr n
    ppr (NumT nt) = text "#" <> ppr nt
    ppr UnknownT = text "?"

