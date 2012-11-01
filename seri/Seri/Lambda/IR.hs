-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | Definition of the abstract syntax for the Seri core lambda expressions.
module Seri.Lambda.IR (
    Name, name, unname, ncons, ntake, nappend, nhead, ntail, nnull,
    TyVar(..), NType(..), Type(..),
    Sig(..), TopSig(..), Class(..), Context,
    Pat(..), Match(..),
    Lit(..), Exp(..), 
    Con(..), Method(..), Dec(..),
    isDataD, tyVarType, tyVarName, nteval,
    ) where

import qualified Data.ByteString.Char8 as STR

type Name = STR.ByteString

name :: String -> Name
name = STR.pack

unname :: Name -> String
unname = STR.unpack

ncons :: Char -> Name -> Name
ncons = STR.cons

nhead :: Name -> Char
nhead = STR.head

ntail :: Name -> Name
ntail = STR.tail

nnull :: Name -> Bool
nnull = STR.null

ntake :: Int -> Name -> Name
ntake = STR.take

nappend :: Name -> Name -> Name
nappend = STR.append

type NTOp = String

-- | Numeric types.
data NType = ConNT Integer   -- ^ numeric type (should be non-negative)
           | VarNT Name      -- ^ numeric type variable
           | AppNT NTOp NType NType -- ^ numeric type operator application
       deriving (Eq, Ord, Show)

data Type = ConT Name                       -- ^ type constructor
          | AppT Type Type                  -- ^ type application
          | VarT Name                       -- ^ type variable
          | NumT NType                      -- ^ numeric type
          | UnknownT
      deriving(Eq, Ord, Show)

-- | 'Sig' is a name annotated with a type.
data Sig = Sig Name Type
    deriving(Eq, Ord, Show)

-- | 'TopSig' is a signature with a context.
data TopSig = TopSig Name Context Type
    deriving(Eq, Ord, Show)

type Context = [Class]

-- | 'Class' represents a single predicate.
-- For example, the predicate (MonadState s m) is represented with:
-- > Class "MonadState" [VarT "s", VarT "m"]
data Class = Class Name [Type]
      deriving(Eq, Ord, Show)

data Match = Match [Pat] Exp      -- ^ p1, p2, ... -> e
    deriving (Eq, Ord, Show)

data Lit = IntegerL Integer         -- ^ integer literal
         | CharL Char               -- ^ character literal
    deriving (Eq, Ord, Show)

data Exp = LitE Lit                 -- ^ literal
         | ConE Sig                 -- ^ data constructor
         | VarE Sig                 -- ^ variable
         | LaceE [Match]            -- ^ lambda-case
         | AppE Exp [Exp]             -- ^ f x y ...
     deriving (Eq, Ord, Show)

-- | Patterns.
-- Note: The Type of ConP is the type of the fully applied Pattern, not the
-- type of the constructor.
data Pat = ConP Type Name [Pat]
         | VarP Sig
         | LitP Lit
         | WildP Type
     deriving (Eq, Ord, Show)

data Con = Con Name [Type]
    deriving(Eq, Ord, Show)

data Method = Method Name Exp
    deriving(Eq, Ord, Show)

data TyVar = NormalTV Name
           | NumericTV Name
       deriving (Eq, Ord, Show)

data Dec = ValD TopSig Exp              -- ^ nm :: ctx => ty ; nm = exp
         | DataD Name [TyVar] [Con]      -- ^ data nm vars = 
         | ClassD Name [TyVar] [TopSig]  -- ^ class nm vars where { sigs }
         | InstD Context Class [Method] -- ^ instance ctx => cls where { meths }
         | PrimD TopSig                 -- ^ nm :: ctx => ty ;
     deriving (Eq, Ord, Show)

-- | Return True if the declaration is a Data Declaration
isDataD :: Dec -> Bool
isDataD (DataD {}) = True
isDataD _ = False

-- | Convert a type variable to a variable type.
tyVarType :: TyVar -> Type
tyVarType (NormalTV n) = VarT n
tyVarType (NumericTV n) = NumT (VarNT n)

-- | Get the name of a type variable
tyVarName :: TyVar -> Name
tyVarName (NormalTV n) = n
tyVarName (NumericTV n) = n

-- | Evaluate a concrete numeric type.
nteval :: NType -> Integer
nteval (ConNT i) = i
nteval v@(VarNT {}) = error $ "nteval: non-concrete numeric type: " ++ show v
nteval (AppNT "+" a b) = nteval a + nteval b
nteval (AppNT "-" a b) = nteval a - nteval b
nteval (AppNT "*" a b) = nteval a * nteval b
nteval (AppNT f a b) = error $ "nteval: unknown AppNT op: " ++ f

