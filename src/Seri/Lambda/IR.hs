
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
         | PrimE Sig
         | CaseE Exp [Match]
         | AppE Exp Exp
         | LamE Sig Exp
         | ConE Sig
         | VarE Sig VarInfo
     deriving (Eq, Show, Data, Typeable)

data Pat = ConP Sig
         | VarP Sig
         | IntegerP Integer
         | AppP Pat Pat
         | WildP Type
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

instance Ppr Exp where
    ppr (IntegerE i) = integer i
    ppr (PrimE s) = text "@" <> ppr s
    ppr (AppE a b) = parens $ ppr a <+> ppr b
    ppr (CaseE e ms) = text "case" <+> ppr e <+> text "of" <+> ppr ms
    ppr (LamE s b) = parens $ text "\\" <> ppr s <+> text "->" <+> ppr b
    ppr (ConE s) = ppr s
    ppr (VarE s _) = ppr s

instance Ppr Match where
    ppr (Match p e) = ppr p <+> text "->" <+> ppr e

instance Ppr Pat where
    ppr (ConP s) = ppr s
    ppr (VarP s) = ppr s
    ppr (IntegerP i) = integer i
    ppr (AppP a b) = ppr a <+> ppr b
    ppr (WildP _) = text "_"

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
    ppr (Sig n t) = text n <> braces (ppr t)

instance Ppr Method where
    ppr (Method n e) = text n <+> text "=" <+> ppr e

