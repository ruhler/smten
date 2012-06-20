
{-# LANGUAGE DeriveDataTypeable #-}

-- | Definition of the abstract syntax for the Seri core lambda expressions.
module Seri.Lambda.IR (
    Name, Type(..), Sig(..), Class(..), 
    Pat(..), Match(..),
    VarInfo(..), Exp(..), 
    Con(..), Method(..), Dec(..),
    isDataD,
    ) where

import Data.Generics

type Name = String

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name                       -- ^ type variable
          | ForallT [Name] [Class] Type     -- ^ forall vars . ctx => type
          | UnknownT
      deriving(Eq, Show, Data, Typeable)

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Show, Data, Typeable)


-- | 'Class' represents a single predicate.
-- For example, the predicate (MonadState s m) is represented with:
-- > Class "MonadState" [VarT "s", VarT "m"]
data Class = Class Name [Type]
      deriving(Eq, Show, Data, Typeable)

-- | 'VarInfo' 
-- Information about a variable.
-- [@Bound@] The variable is locally bound by a lambda or pattern match.
-- [@Declared@] The variable refers to a top level declaration.
-- [@Instance@] The variable refers to a method of the given class instance.
-- [@UnknownVI@] The information about the variable is unknown.
data VarInfo = Bound | Declared | Instance Class | UnknownVI
    deriving (Eq, Show, Data, Typeable)

data Match = Match Pat Exp      -- ^ p -> e
    deriving (Eq, Show, Data, Typeable)

data Exp = IntegerE Integer         -- ^ integer literal
         | PrimE Sig                -- ^ primitive 
         | CaseE Exp [Match]        -- ^ case e of { ms }
         | AppE Exp Exp             -- ^ f x
         | LamE Sig Exp             -- ^ \x -> e
         | ConE Sig                 -- ^ data constructor
         | VarE Sig VarInfo         -- ^ variable
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

data Dec = ValD Sig Exp               -- ^ nm :: ty ; nm = exp
         | DataD Name [Name] [Con]    -- ^ data nm vars = 
         | ClassD Name [Name] [Sig]   -- ^ class nm vars where { sigs }
         | InstD Class [Method]       -- ^ instance cls where { meths }
     deriving (Eq, Show, Data, Typeable)

-- | Return True if the declaration is a Data Declaration
isDataD :: Dec -> Bool
isDataD (DataD {}) = True
isDataD _ = False

