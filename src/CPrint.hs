
module CPrint (
    Ppr(..)
    ) where

import Language.Haskell.TH.PprLib
import Language.Haskell.TH(Ppr(..))

import C

instance Ppr Type where
    ppr IntT = text "int"

commajoin :: [Doc] -> Doc
commajoin [x] = x
commajoin (x:xs) = x <> comma <+> commajoin xs

instance Ppr Exp where
    ppr (IntE i) = integer i
    ppr (AddE a b) = parens $ ppr a <+> text "+" <+> ppr b
    ppr (MulE a b) = parens $ ppr a <+> text "*" <+> ppr b
    ppr (AppE f args) = ppr f <> (parens . commajoin $ map ppr args)
    ppr (VarE n) = text n

instance Ppr Stmt where
    ppr (ReturnS e) = text "return" <+> ppr e <> semi

instance Ppr Dec where
    ppr (FunD name args rtype body)
        = ppr rtype
            <+> text name
            <> parens (commajoin (map (\(t, n) -> ppr t <+> text n) args))
            $+$ lbrace $+$ (nest 4 (vcat (map ppr body))) $+$ rbrace

