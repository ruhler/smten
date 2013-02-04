
{-# LANGUAGE PatternGuards #-}

module Seri.Type.Ppr () where

import Seri.Ppr
import Seri.Type.Type
import Seri.Type.Sugar

instance Ppr NType where
    ppr (ConNT i) = integer i
    ppr (VarNT v) = ppr v
    ppr (AppNT o a b) = parens (ppr a <+> text o <+> ppr b)

-- Print an atomic type.
-- Wraps complex types in parens.
atom :: Type -> Doc
atom t | Just _ <- de_listT t = ppr t
atom t | Just _ <- de_arrowT t = parens (ppr t)
atom t | Just _ <- de_appT t = parens (ppr t)
atom t = ppr t

instance Ppr Type where
    ppr t | Just v <- de_listT t = text "[" <> ppr v <> text "]"
    ppr t | Just vs <- de_tupleT t
      = text "(" <> sep (punctuate comma (map ppr vs)) <> text ")"
    ppr t | (t1:ts@(_:_)) <- de_arrowsT t   
      = sep $ atom t1 : concat [[text "->", atom tx] | tx <- ts]

    ppr (ConT n) = ppr n
    ppr (AppT a b) = atom a <+> atom b
    ppr (VarT n) = ppr n
    ppr (NumT nt) = text "#" <> ppr nt
    ppr UnknownT = text "?"

