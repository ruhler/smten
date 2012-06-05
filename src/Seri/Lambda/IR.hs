
{-# LANGUAGE DeriveDataTypeable #-}

module Seri.Lambda.IR (
    Name, Type(..), Pat(..), Match(..), Exp(..), Dec(..), Con(..),
    Sig(..), Method(..), VarInfo(..), Pred(..),
    ) where

import Data.Generics

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

data Dec = ValD Sig Exp
         | DataD Name [Name] [Con]    -- name tyvars constrs
         | ClassD Name [Name] [Sig]   -- name tyvars sigs
         | InstD Name [Type] [Method]
     deriving (Eq, Show, Data, Typeable)

