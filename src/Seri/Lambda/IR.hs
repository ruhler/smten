
{-# LANGUAGE DeriveDataTypeable #-}

-- | Definition of the abstract syntax for the Seri core lambda expressions.
module Seri.Lambda.IR (
    Name, Type(..), Sig(..), TopSig(..), Class(..), Context,
    Pat(..), Match(..),
    Exp(..), 
    Con(..), Method(..), Dec(..),
    isDataD,
    ) where

import Data.Generics

type Name = String

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name                       -- ^ type variable
          | UnknownT
      deriving(Eq, Show, Data, Typeable)

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Show, Data, Typeable)

-- | 'TopSig' is a signature with a context.
data TopSig = TopSig Name Context Type
    deriving(Eq, Show, Data, Typeable)

type Context = [Class]

-- | 'Class' represents a single predicate.
-- For example, the predicate (MonadState s m) is represented with:
-- > Class "MonadState" [VarT "s", VarT "m"]
data Class = Class Name [Type]
      deriving(Eq, Show, Data, Typeable)

data Match = Match Pat Exp      -- ^ p -> e
    deriving (Eq, Show, Data, Typeable)

data Exp = IntegerE Integer         -- ^ integer literal
         | CaseE Exp [Match]        -- ^ case e of { ms }
         | AppE Exp Exp             -- ^ f x
         | LamE Sig Exp             -- ^ \x -> e
         | ConE Sig                 -- ^ data constructor
         | VarE Sig                 -- ^ variable
     deriving (Eq, Show, Data, Typeable)

-- | Patterns.
-- Note: The Type of ConP is the type of the fully applied Pattern, not the
-- type of the constructor.
data Pat = ConP Type Name [Pat]
         | VarP Sig
         | IntegerP Integer
         | WildP Type
     deriving (Eq, Show, Data, Typeable)

data Con = Con Name [Type]
    deriving(Eq, Show, Data, Typeable)

data Method = Method Name Exp
    deriving(Eq, Show, Data, Typeable)

data Dec = ValD TopSig Exp              -- ^ nm :: ctx => ty ; nm = exp
         | DataD Name [Name] [Con]      -- ^ data nm vars = 
         | ClassD Name [Name] [TopSig]  -- ^ class nm vars where { sigs }
         | InstD Context Class [Method] -- ^ instance ctx => cls where { meths }
         | PrimD TopSig                 -- ^ nm :: ctx => ty ;
     deriving (Eq, Show, Data, Typeable)

-- | Return True if the declaration is a Data Declaration
isDataD :: Dec -> Bool
isDataD (DataD {}) = True
isDataD _ = False

