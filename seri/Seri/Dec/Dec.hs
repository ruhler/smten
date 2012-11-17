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

-- | Seri declarations.
module Seri.Dec.Dec (
    TyVar(..), tyVarType, tyVarName,
    TopSig(..), Class(..), Context,
    Con(..), Method(..), Dec(..),
    ) where

import Seri.Name
import Seri.Type
import Seri.Lit
import Seri.Sig
import Seri.Exp

-- | 'TopSig' is a signature with a context.
data TopSig = TopSig Name Context Type
    deriving(Eq, Show)

type Context = [Class]

-- | 'Class' represents a single predicate.
-- For example, the predicate (MonadState s m) is represented with:
-- > Class "MonadState" [VarT "s", VarT "m"]
data Class = Class Name [Type]
      deriving(Eq, Show)

data Con = Con Name [Type]
    deriving(Eq, Show)

data Method = Method Name Exp
    deriving(Eq, Show)

data TyVar = NormalTV Name
           | NumericTV Name
       deriving (Eq, Ord, Show)

data Dec = ValD TopSig Exp              -- ^ nm :: ctx => ty ; nm = exp
         | DataD Name [TyVar] [Con]      -- ^ data nm vars = 
         | ClassD Name [TyVar] [TopSig]  -- ^ class nm vars where { sigs }
         | InstD Context Class [Method] -- ^ instance ctx => cls where { meths }
         | PrimD TopSig                 -- ^ nm :: ctx => ty ;
     deriving (Eq, Show)

-- | Convert a type variable to a variable type.
tyVarType :: TyVar -> Type
tyVarType (NormalTV n) = VarT n
tyVarType (NumericTV n) = NumT (VarNT n)

-- | Get the name of a type variable
tyVarName :: TyVar -> Name
tyVarName (NormalTV n) = n
tyVarName (NumericTV n) = n
