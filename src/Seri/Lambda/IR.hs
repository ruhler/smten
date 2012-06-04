
{-# LANGUAGE DeriveDataTypeable #-}

module Seri.Lambda.IR (
    Name, Type(..), Pat(..), Match(..), Exp(..), Dec(..), Con(..),
    Sig(..), Method(..), VarInfo(..), Pred(..),
    ) where

import Data.List(nub)
import Data.Generics

import Seri.Utils.Ppr

type Name = String

data Type = ConT Name
          | AppT Type Type
          | VarT Name
          | ForallT [Name] [Pred] Type     -- tyvars ctx type
      deriving(Eq, Show, Data, Typeable)

data Pred = Pred Name [Type]
      deriving(Eq, Show, Data, Typeable)

data VarInfo = Bound | Declared | Instance Name [Type]
    deriving (Eq, Show, Data, Typeable)

data Match = Match Pat Exp
    deriving (Eq, Show, Data, Typeable)

data Exp = IntegerE Integer
         | PrimE Type Name
         | CaseE Type Exp [Match]
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | ConE Type Name
         | VarE Type Name VarInfo
     deriving (Eq, Show, Data, Typeable)

data Pat = ConP Name
         | VarP Name
         | IntegerP Integer
         | AppP Pat Pat
         | WildP
     deriving (Eq, Show, Data, Typeable)

data Con = Con Name [Type]
    deriving(Eq, Show, Data, Typeable)

data Sig = Sig Name Type
    deriving(Eq, Show, Data, Typeable)

data Method = Method Name Exp
    deriving(Eq, Show, Data, Typeable)

data Dec = ValD Name Type Exp
         | DataD Name [Name] [Con]    -- name tyvars constrs
         | ClassD Name [Name] [Sig]   -- name tyvars sigs
         | InstD Name [Type] [Method]
     deriving (Eq, Show, Data, Typeable)

instance Ppr Type where
    ppr (ConT nm) = text nm
    ppr (AppT a b) = parens $ ppr a <+> ppr b
    ppr (VarT n) = text n
    ppr (ForallT ns ps t) =
        text "forall" <+> hsep (map text ns) <+> hsep (map ppr ps) <+> ppr t

instance Ppr Pred where
    ppr (Pred n ts) = text n <+> hsep (map ppr ts)

pprtype :: Type -> Doc -> Doc
pprtype t x = parens $ x <+> text "::" <+> ppr t

instance Ppr Exp where
    ppr (IntegerE i) = integer i
    ppr (PrimE t p) = pprtype t (text "@" <> text p)
    ppr (AppE _ a b) = parens $ ppr a <+> ppr b
    ppr (CaseE _ e ms) = text "case" <+> ppr e <+> text "of" <+> ppr ms
    ppr (LamE t n b) = pprtype t (parens $ text "\\" <> text n <+> text "->" <+> ppr b)
    ppr (ConE t n) = pprtype t (text n)
    ppr (VarE t n _) = pprtype t (text n)

instance Ppr Match where
    ppr (Match p e) = ppr p <+> text "->" <+> ppr e

instance Ppr Pat where
    ppr (ConP nm) = text nm
    ppr (VarP nm) = text nm
    ppr (IntegerP i) = integer i
    ppr (AppP a b) = ppr a <+> ppr b
    ppr WildP = text "_"

instance Ppr Con where
    ppr (Con n ts) = text n <+> hsep (map ppr ts)

instance Ppr Dec where
    ppr (ValD n t e) = text n <+> text "::" <+> ppr t
                        $+$ text n <+> text "=" <+> ppr e
    ppr (DataD n vs cs) = text "data" <+> text n <+> hsep (map text vs) <+> text "=" <+> ppr cs
    ppr (ClassD n vs sigs) 
        = text "class" <+> text n <+> hsep (map text vs) <+> text "where"
            <+> ppr sigs
    ppr (InstD n ts meths)
        = text "instance" <+> text n <+> hsep (map ppr ts) <+> text "where"
            <+> ppr meths

instance Ppr Sig where
    ppr (Sig n t) = text n <+> text "::" <+> ppr t 

instance Ppr Method where
    ppr (Method n e) = text n <+> text "=" <+> ppr e

