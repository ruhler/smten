
{-# LANGUAGE FlexibleInstances #-}

module Seri.Lambda.Ppr () where

import Data.List(group, nub)

import Seri.Lambda.IR
import Seri.Lambda.Sugar
import Seri.Utils.Ppr

tabwidth :: Int
tabwidth = 2

isAtomT :: Type -> Bool
isAtomT (AppT (ConT "[]") _) = True
isAtomT (AppT (AppT (ConT "(,)") a) b) = True
isAtomT (AppT (AppT (AppT (ConT "(,,)") a) b) c) = True
isAtomT (ConT {}) = True
isAtomT (VarT {}) = True
isAtomT (AppT {}) = False
isAtomT (ForallT {}) = False
isAtomT (UnknownT {}) = True

isArrowsT :: Type -> Bool
isArrowsT (AppT (AppT (ConT "->") _) _) = True
isArrowsT _ = False

instance Ppr Type where
    -- Special case for list
    ppr (AppT (ConT "[]") t) = text "[" <> ppr t <> text "]"
    
    -- Special case for tuples
    ppr (AppT (AppT (ConT "(,)") a) b)
        = parens . sep $ punctuate comma (map ppr [a, b])
    ppr (AppT (AppT (AppT (ConT "(,,)") a) b) c)
        = parens . sep $ punctuate comma (map ppr [a, b, c])
    ppr (AppT (AppT (AppT (AppT (ConT "(,,,)") a) b) c) d)
        = parens . sep $ punctuate comma (map ppr [a, b, c, d])

    -- Special case for ->
    ppr (AppT (AppT (ConT "->") a) b) | isArrowsT a
        = parens (ppr a) <+> text "->" <+> ppr b
    ppr (AppT (AppT (ConT "->") a) b)
        = ppr a <+> text "->" <+> ppr b
    ppr (ConT "->") = text "(->)"

    -- Normal cases
    ppr (ConT n) = text n
    ppr (VarT n) = text n
    ppr (AppT a b) | isAtomT b = ppr a <+> ppr b
    ppr (AppT a b) = ppr a <+> (parens $ ppr b)
    ppr (ForallT vars preds t)
      = let ctx [] = empty
            ctx _ = (parens . sep $ punctuate comma (map ppr preds)) <+> text "=>" 
        in text "forall" <+> hsep (map text vars)
              <+> text "." <+> ctx preds <+> ppr t
    ppr UnknownT = text "?"


isAtomE :: Exp -> Bool
isAtomE (IntegerE {}) = True
isAtomE (PrimE {}) = True
isAtomE (CaseE {}) = False
isAtomE (AppE {}) = False
isAtomE (LamE {}) = False
isAtomE (ConE {}) = True
isAtomE (VarE {}) = True

isOp :: String -> Bool
isOp (c:_) = c `elem` ":!#$%&*+./<=>?@\\^|-~"

pprname :: String -> Doc
pprname n | isOp n = parens (text n)
pprname n = text n

pprsig :: Doc -> Sig -> Doc
pprsig s (Sig n UnknownT) = pprname n
pprsig s (Sig n t) = parens (s <> pprname n <+> text "::" <+> (ppr t))

sep2 :: Doc -> Doc -> Doc
sep2 a b = a $$ nest tabwidth b

cando :: Exp -> Bool
cando (AppE (AppE (VarE (Sig ">>" _) _) _) _) = True
cando (AppE (AppE (VarE (Sig ">>=" _) _) _) (LamE _ _)) = True
cando _ = False

sugardo :: Exp -> (Class, [Stmt])
sugardo (AppE (AppE (VarE (Sig ">>" _) (Instance cls)) m) r) =
  let (_, substmts) = sugardo r
  in (cls, NoBindS m : substmts)
sugardo (AppE (AppE (VarE (Sig ">>=" _) (Instance cls)) m) (LamE s r)) =
  let (_, substmts) = sugardo r
  in (cls, BindS s m : substmts)
sugardo e = (undefined, [NoBindS e])

instance Ppr Exp where
    -- Special case for if expressions
    ppr (CaseE e [Match (ConP (Sig "True" (ConT "Bool")) []) a,
                  Match (ConP (Sig "False" (ConT "Bool")) []) b])
        = text "if" <+> ppr e $$ nest tabwidth (
                text "then" <+> ppr a $$
                text "else" <+> ppr b)

    -- Special case for do statements
    ppr e | cando e =
        let (cls, stmts) = sugardo e
        in text "#" <>  braces (ppr cls) <> text "do" <+> text "{" $$
              nest tabwidth (vcat (map ppr stmts)) $$ text "}"

    -- Special case for tuples
    ppr (AppE (AppE (ConE (Sig "(,)" _)) a) b) =
        parens . sep $ punctuate comma (map ppr [a, b])
    ppr (AppE (AppE (AppE (ConE (Sig "(,,)" _)) a) b) c) =
        parens . sep $ punctuate comma (map ppr [a, b, c])
    ppr (AppE (AppE (AppE (AppE (ConE (Sig "(,,,)" _)) a) b) c) d) =
        parens . sep $ punctuate comma (map ppr [a, b, c, d])

    -- Normal cases
    ppr (IntegerE i) = integer i
    ppr (PrimE s) = pprsig (text "@") s
    ppr (CaseE e ms) = text "case" <+> ppr e <+> text "of" <+> text "{" $$
                            nest tabwidth (vcat (map ppr ms)) $$ text "}"
    ppr (AppE a b) | isAtomE b = ppr a <+> ppr b
    ppr (AppE a b) = ppr a <+> (parens $ ppr b)
    ppr (LamE s b) = parens $ (text "\\" <> pprsig empty s <+> text "->") `sep2` ppr b
    ppr (ConE s) = pprsig empty s
    ppr (VarE s Bound) = pprsig (text ".") s
    ppr (VarE s Declared) = pprsig (text "%") s
    ppr (VarE s (Instance cls))
        = pprsig (text "#" <>  braces (ppr cls)) s
    ppr (VarE s UnknownVI) = pprsig empty s

instance Ppr Stmt where
    ppr (NoBindS e) = ppr e <> semi
    ppr (BindS s e) = pprsig empty s <+> text "<-" <+> ppr e <> semi

instance Ppr Match where
    ppr (Match p e) = (ppr p <+> text "->") `sep2` ppr e <> semi

isAtomP :: Pat -> Bool
isAtomP (ConP _ []) = True
isAtomP (ConP _ _) = False
isAtomP (VarP {}) = True
isAtomP (IntegerP {}) = True
isAtomP (WildP {}) = True

isTuple :: String -> Bool
isTuple n = ["(", ",", ")"] == nub (group n)

instance Ppr Pat where
    -- special case for tuples
    ppr (ConP (Sig n _) ps) | isTuple n = 
        parens . sep $ punctuate comma (map ppr ps)
        
    -- normal cases
    ppr (ConP s ps) =
        let subp p | isAtomP p = ppr p
            subp p = parens (ppr p)
        in pprsig empty s <+> hsep (map subp ps)
    ppr (VarP s) = pprsig empty s
    ppr (IntegerP i) = integer i
    ppr (WildP t) = pprsig empty (Sig "_" t)


conlist :: [Con] -> Doc
conlist [] = empty
conlist (x:xs) = text " " <+> ppr x
                    $+$ vcat (map (\c -> text "|" <+> ppr c) xs)

instance Ppr Dec where
    ppr (ValD (Sig n t) e) = pprname n <+> text "::" <+> ppr t <> semi $$
                             pprname n <+> text "=" <+> ppr e <> semi
    ppr (DataD n vs cs)
        = text "data" <+> text n <+> hsep (map text vs) <+> text "=" $$
            (nest tabwidth (conlist cs)) <> semi
    ppr (ClassD n vs ss)
        = text "class" <+> text n <+> hsep (map text vs)
                <+> text "where" <+> text "{" $$
                    nest tabwidth (vcat (map ppr ss)) $$ text "}" <> semi
    ppr (InstD cls ms)
        = text "instance" <+> ppr cls
                <+> text "where" <+> text "{" $$
                    nest tabwidth (vcat (map ppr ms)) $$ text "}" <> semi

instance Ppr Sig where
    ppr (Sig n t) = pprname n <+> text "::" <+> ppr t <> semi
    
instance Ppr Con where
    ppr (Con n ts) = text n <+> hsep (map ppr ts)

instance Ppr Method where
    ppr (Method n e) = pprname n <+> text "=" <+> ppr e <> semi

instance Ppr [Dec] where
    ppr ds = vcat (map (\d -> ppr d $+$ text "") ds)

instance Ppr Class where
    ppr (Class n ts) = text n <+> hsep (map ppr ts)
