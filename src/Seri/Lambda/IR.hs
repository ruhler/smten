
{-# LANGUAGE DeriveDataTypeable #-}

module Seri.Lambda.IR (
    Name, Type(..), Pat(..), Match(..), Exp(..), Dec(..), Con(..),
    Sig(..), Method(..), VarInfo(..), Class(..),
    ) where

import Data.Generics

type Name = String

data Type = ConT Name
          | AppT Type Type
          | VarT Name
          | ForallT [Name] [Class] Type     -- tyvars ctx type
          | UnknownT
      deriving(Eq, Show, Data, Typeable)

data Class = Class Name [Type]
      deriving(Eq, Show, Data, Typeable)

data VarInfo = Bound | Declared | Instance Class | UnknownVI
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

-- Note: The Type of ConP is the output type of the Type of the signature.
--    For example: ConP (Sig "Foo" (Integer -> Food) [5]) has type Food.
--    (This makes sense for legacy reasons. It's not clear to me if it
--    continues to make sense now. Why not just have: ConP Type Name [Pat],
--    where the type is the data type the constructor belongs to? Maybe just
--    because it's more consistent with all the other kinds of names we type.)
data Pat = ConP Sig [Pat]
         | VarP Sig
         | IntegerP Integer
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
         | InstD Class [Method]
     deriving (Eq, Show, Data, Typeable)

