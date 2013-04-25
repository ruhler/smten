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

{-# LANGUAGE PatternGuards #-}

module Smten.Module.Module (
    Module(..), Exports(..), ImportSpec(..), Import(..),
    Synonym(..), DataDec(..), Deriving(..),
    environ, sderive,
    ) where

import qualified Smten.HashTable as HT

import Smten.Failable
import Smten.Location
import Smten.Name
import Smten.Type
import Smten.Dec

data ImportSpec = Include [Name] | Exclude [Name]
    deriving (Show, Eq)

data Exports = Local | Exports [Name]
    deriving (Show, Eq)

data Import = Import { 
    imp_from :: Name,
    imp_as :: Name,

    -- Import entities from the module qualified only.
    imp_qonly :: Bool,
    imp_spec :: ImportSpec
} deriving(Show, Eq)

-- type Foo a b ... = ...
data Synonym = Synonym Name [Name] Type 
    deriving (Show, Eq)

-- data Foo a b ... = ...
data DataDec = DataDec Name [TyVar] [ConRec]
    deriving (Show, Eq)

-- deriving instance ctx => cls
data Deriving = Deriving Location Context Class
    deriving (Show, Eq)

data Module = Module {
    mod_name :: Name,
    mod_exports :: Exports,
    mod_imports :: [Import],
    mod_synonyms :: [Synonym],

    -- | A copy of the original data type declarations in the module. This is
    -- recorded here so we have access to the record type constructors for
    -- stand-alone deriving.
    -- Note: mod_decs already contains the desugared declarations
    -- corresponding to this data declaration.
    mod_ddecs :: [DataDec],

    -- | list of stand-alone deriving only.
    mod_derivings :: [Deriving],

    mod_decs :: [Dec]
} deriving(Show, Eq)

mkDDecs :: [DataDec] -> HT.HashTable Name [ConRec]
mkDDecs xs = HT.table [(n, cs) | DataDec n _ cs <- xs]

-- Perform standalone derivings in the given modules.
sderive :: [Module] -> Failable [Module]
sderive ms = {-# SCC "StandAloneDerive" #-} do
  let ddecs = mkDDecs $ concatMap mod_ddecs ms

      mderive :: Deriving -> Failable Dec
      mderive d@(Deriving loc ctx cls)
        | Class _ [t] <- cls
        , (ct, _) <- de_appsT t
        , Just n <- de_conT ct
        , Just cs <- HT.lookup n ddecs = return (derive loc ctx cls cs)
        | otherwise = throw $ lmsg loc "unable to perform standalone derive"

      sd1 :: Module -> Failable Module
      sd1 m = do
          derives <- mapM mderive (mod_derivings m)
          return $ m { mod_decs = derives ++ mod_decs m }
  mapM sd1 ms
  
environ :: [Module] -> Env
environ = mkEnv . concat . map mod_decs

