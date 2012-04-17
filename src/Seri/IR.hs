
module Seri.IR (
    Name, Type(..), Pat(..), Match(..), Exp(..), Dec(..), Con(..),
    Sig(..), Method(..), InstId(..), Pred(..),
    ) where

import Data.List(nub)

import Seri.Ppr

type Name = String

data Type = ConT Name
          | ArrowT
          | AppT Type Type
          | VarT Name
          | ForallT [Name] [Pred] Type     -- tyvars ctx type
      deriving(Eq, Show)

data Pred = Pred Name [Type]
      deriving(Eq, Show)

data InstId = NoInst | Inst Name [Type]
    deriving (Eq, Show)

data Match = Match Pat Exp
    deriving (Eq, Show)

data Exp = IntegerE Integer
         | PrimE Type Name
         | IfE Type Exp Exp Exp
         | CaseE Type Exp [Match]
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | ConE Type Name
         | VarE Type Name InstId
     deriving (Eq, Show)

data Pat = ConP Name
         | VarP Name
         | IntegerP Integer
         | AppP Pat Pat
         | WildP
     deriving (Eq, Show)

data Con = Con Name [Type]
    deriving(Eq, Show)

data Sig = Sig Name Type
    deriving(Eq, Show)

data Method = Method Name Exp
    deriving(Eq, Show)

data Dec = ValD Name Type Exp
         | DataD Name [Name] [Con]    -- name tyvars constrs
         | ClassD Name [Name] [Sig]   -- name tyvars sigs
         | InstD Name [Type] [Method]
     deriving (Eq, Show)

instance Ppr Type where
    ppr (ConT nm) = text nm
    ppr ArrowT = text "->"
    ppr (AppT a b) = parens $ ppr a <+> ppr b
    ppr (VarT n) = text n

instance Ppr Exp where
    ppr (IntegerE i) = integer i
    ppr (PrimE _ p) = text p
    ppr (IfE _ p a b) = parens $ text "if" <+> ppr p
                        <+> text "then" <+> ppr a
                        <+> text "else" <+> ppr b
    ppr (AppE _ a b) = parens $ ppr a <+> ppr b
    ppr (CaseE _ e ms) = text "case" <+> ppr e <+> text "of" <+> ppr ms
    ppr (LamE _ n b) = parens $ text "\\" <> text n <+> text "->" <+> ppr b
    ppr (ConE _ n) = text n
    ppr (VarE _ n _) = text n

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

