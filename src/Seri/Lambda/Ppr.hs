
{-# LANGUAGE FlexibleInstances #-}

module Seri.Lambda.Ppr () where

import Seri.Lambda.IR
import Seri.Utils.Ppr

tabwidth :: Int
tabwidth = 2

isAtomT :: Type -> Bool
isAtomT (ConT {}) = True
isAtomT (VarT {}) = True
isAtomT (AppT {}) = False
isAtomT (ForallT {}) = False

instance Ppr Type where
    ppr (ConT n) = text n
    ppr (VarT n) = text n
    ppr (AppT a b) | isAtomT b = ppr a <+> ppr b
    ppr (AppT a b) = ppr a <+> (parens $ ppr b)
    ppr (ForallT vars preds t)
      = let ctx [] = empty
            ctx _ = (parens . sep $ punctuate comma (map ppr preds)) <+> text "=>" 
        in text "forall" <+> hsep (map text vars)
              <+> text "." <+> ctx preds <+> ppr t

instance Ppr Pred where
    ppr (Pred n ts) = text n <+> hsep (map ppr ts)

isAtomE :: Exp -> Bool
isAtomE (IntegerE {}) = True
isAtomE (PrimE {}) = True
isAtomE (CaseE {}) = True
isAtomE (AppE {}) = False
isAtomE (LamE {}) = False
isAtomE (ConE {}) = True
isAtomE (VarE {}) = True

pprsig :: String -> Sig -> Doc
pprsig s (Sig n t) = text n <> text s <> braces (ppr t)

sep2 :: Doc -> Doc -> Doc
sep2 a b = a $$ nest tabwidth b

instance Ppr Exp where
    ppr (IntegerE i) = integer i
    ppr (PrimE s) = pprsig "@" s
    ppr (CaseE e ms) = text "case" <+> ppr e <+> text "of" <+> text "{" $$
                            nest tabwidth (vcat (map ppr ms)) $$ text "}"
    ppr (AppE a b) | isAtomE b = ppr a <+> ppr b
    ppr (AppE a b) = ppr a <+> (parens $ ppr b)
    ppr (LamE s b) = (text "\\" <> pprsig "." s <+> text "->") `sep2` ppr b
    ppr (ConE s) = pprsig "%" s
    ppr (VarE s Bound) = pprsig "." s
    ppr (VarE s Declared) = pprsig "%" s
    ppr (VarE (Sig n t) (Instance ni tis))
        = text n <> text "#" <> braces (ppr t <> comma <+> ppr (Pred ni tis))

instance Ppr Match where
    ppr (Match p e) = (ppr p <+> text "->") `sep2` ppr e <> semi

isAtomP :: Pat -> Bool
isAtomP (ConP {}) = True
isAtomP (VarP {}) = True
isAtomP (IntegerP {}) = True
isAtomP (AppP {}) = False
isAtomP (WildP {}) = True

instance Ppr Pat where
    ppr (ConP s) = pprsig "%" s
    ppr (VarP s) = pprsig "%" s
    ppr (IntegerP i) = integer i
    ppr (AppP a b) | isAtomP b = ppr a <+> ppr b
    ppr (AppP a b) = ppr a <+> (parens $ ppr b)
    ppr (WildP t) = text "_" <+> braces (ppr t)


conlist :: [Con] -> Doc
conlist [] = empty
conlist (x:xs) = text " " <+> ppr x
                    $+$ vcat (map (\c -> text "|" <+> ppr c) xs)

instance Ppr Dec where
    ppr (ValD s e) = pprsig "%" s <+> text "=" $$ nest tabwidth (ppr e <> semi)
    ppr (DataD n vs cs)
        = text "data" <+> text n <+> hsep (map text vs) <+> text "=" $$
            nest tabwidth (conlist cs)
    ppr (ClassD n vs ss)
        = text "class" <+> text n <+> hsep (map text vs) <+> text "where" $$
            nest tabwidth (vcat (map (pprsig "#") ss))
    ppr (InstD n ts ms)
        = text "instance" <+> ppr (Pred n ts) <+> text "where" $$
            vcat (map ppr ms)
    
instance Ppr Con where
    ppr (Con n ts) = text n <+> hsep (map ppr ts)

instance Ppr Method where
    ppr (Method n e) = text n <+> text "=" <+> ppr e

instance Ppr [Dec] where
    ppr ds = vcat (map (\d -> ppr d $+$ text "") ds)
