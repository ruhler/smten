
{-# LANGUAGE PatternGuards #-}

module Smten.Type.Ppr () where

import Smten.Ppr
import Smten.Type.Type
import Smten.Type.Sugar
import Smten.Type.Utils

-- Print an atomic type in an App context
atomApp :: Type -> Doc
atomApp t | Just _ <- de_listT t = ppr t
atomApp t | Just _ <- de_tupleT t = ppr t
atomApp t | Just _ <- de_arrowT t = parens (ppr t)
atomApp t | Just _ <- de_appT t = parens (ppr t)
atomApp t = ppr t

-- Print an atomic type in an Arrow context
atomArrow :: Type -> Doc
atomArrow t | Just _ <- de_listT t = ppr t
atomArrow t | Just _ <- de_tupleT t = ppr t
atomArrow t | Just _ <- de_arrowT t = parens (ppr t)
atomArrow t = ppr t

instance Ppr Type where
    ppr t | Just v <- de_listT t = text "[" <> ppr v <> text "]"
    ppr t | Just vs <- de_tupleT t
      = text "(" <> hsep (punctuate comma (map ppr vs)) <> text ")"
    ppr t | (t1:ts@(_:_)) <- de_arrowsT t   
      = hsep $ atomArrow t1 : concat [[text "->", atomArrow tx] | tx <- ts]
    ppr t | kindof t == NumK = text "#" <> pprnumt t

    ppr (ConT n _) = ppr n
    ppr (AppT a b) = atomArrow a <+> atomApp b
    ppr (VarT n _) = ppr n
    ppr UnknownT = text "?"

pprnumt :: Type -> Doc
pprnumt (NumT i) = integer i
pprnumt (OpT f a b) = parens $ pprnumt a <> text f <> pprnumt b
pprnumt (VarT n _) = ppr n
pprnumt t = ppr t

instance Ppr Kind where
    ppr StarK = text "*"
    ppr NumK = text "#"
    ppr (ArrowK a b) = ppr a <+> text "->" <+> ppr b
    ppr UnknownK = text "?"
    ppr (VarK i) = text "$" <> integer i

