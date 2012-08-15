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

{-# LANGUAGE FlexibleInstances #-}

-- | Hopefully efficient queries regarding a seri environment.
module Seri.Lambda.Env (
    Env(), mkEnv, tweak,
    VarInfo(..),
    lookupVarType, lookupVarValue, lookupVar, lookupVarInfo,
    lookupMethodType,
    lookupDataD, lookupDataConType,
    lookupInstD, lookupPrimD,
    lookupValD, lookupClassD,
    ) where

import Debug.Trace

import Control.Monad

import Data.Maybe

import Seri.Failable
import Seri.HashTable as HT
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types

-- | Env is an abstract data type representing information about a seri
-- environment.
data Env = Env {
    -- | All the original declarations.
    e_decls :: [Dec],

    -- Fast access to info about values.
    e_vitable :: HashTable Name ValInfo,

    -- Fast access to info about data constructors.
    -- Returns the type of a data constructor.
    e_dctable :: HashTable Name Type

}

-- | Information about a value name.
-- TODO: should DataD not be in this table, because we are never confused
-- between a DataD and a value? I think so. I was just lazy when I added DataD
-- here.
data ValInfo 
 = DecVI Dec     -- ^ The value is the given ValD or PrimD or DataD
 | ClassVI Name [TyVar] Type   -- ^ The value belongs to the given class and has given type.
    deriving (Eq, Show)

-- | Build a seri environment from the given list of declarations.
mkEnv :: [Dec] -> Env
mkEnv decs = Env decs (table (vitable decs)) (table (dctable decs))

vitable :: [Dec] -> [(Name, ValInfo)]
vitable decs =
  let videc :: Dec -> [(Name, ValInfo)]
      videc d@(ValD (TopSig n _ _) _) = [(n, DecVI d)]
      videc d@(PrimD (TopSig n _ _)) = [(n, DecVI d)]
      videc (ClassD cn ts sigs) =  
        let isig :: TopSig -> (Name, ValInfo)
            isig (TopSig n _ t) = (n, ClassVI cn ts t)
        in map isig sigs
      videc d@(DataD n _ _) = [(n, DecVI d)]
      videc (InstD {}) = []
  in concat $ map videc decs

dctable :: [Dec] -> [(Name, Type)]
dctable decs =
  let dcdec :: Dec -> [(Name, Type)]
      dcdec d@(DataD dn vars cs) =
        let dccon :: Con -> (Name, Type)
            dccon (Con n ts) = (n, arrowsT (ts ++ [appsT (ConT dn : map tyVarType vars)]))
        in map dccon cs
      dcdec _ = []
  in concat $ map dcdec decs

-- | Make a small change to an environment.
-- Add the given declarations to the environment, overwriting any existing
-- conflicting ones.
--
-- TODO: if HashTable gave us a array-update like function, we could
-- presumibly do this more cheaply.
tweak :: [Dec] -> Env -> Env
tweak tds e = mkEnv (tds ++ e_decls e)

-- | 'VarInfo' 
-- Information about a variable.
-- [@Primitive@] The variable is a primitive.
-- [@Declared@] The variable refers to a top level declaration.
-- [@Instance@] The variable refers to a method of the given class instance.
data VarInfo = Primitive |  Declared | Instance Class
    deriving (Eq, Show)


-- | theOneOf kind name predicate decls
-- Common code for extracting a single declaration from a list.
--  kind - the kind of declaration searched in (for error messages)
--  name - the name of the declaration searched for (for error messages)
--  predicate - a predicate which identifies the desired declaration
--  env - the environment to search in.
theOneOf :: String -> String -> (Dec -> Bool) -> Env -> Failable Dec
theOneOf kind n p e =
    case filter p (e_decls e) of
        [] -> fail $ kind ++ " for " ++ n ++ " not found"
        x -> return $ head x

-- | Look up a ValD with given Name in the given Environment.
lookupValD :: Env -> Name -> Failable Dec
lookupValD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(ValD {})) -> return d
     _ -> fail $ "lookupValD: " ++ n ++ " is not a ValD"

-- | Look up a PrimD with given Name in the given Environment.
lookupPrimD :: Env -> Name -> Failable Dec
lookupPrimD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(PrimD {})) -> return d
     _ -> fail $ "lookupPrimD: " ++ n ++ " is not a PrimD"
 
        
-- | Look up a DataD with given type constructor Name in the given
-- Environment.
lookupDataD :: Env -> Name -> Failable Dec
lookupDataD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(DataD {})) -> return d  
     _ -> fail $ "lookupDataD: " ++ n ++ " is not a DataD"

-- | Look up a ClassD with given Name in the given Environment.
lookupClassD :: Env -> Name -> Failable Dec
lookupClassD env n =
  let theClassD :: Dec -> Bool
      theClassD (ClassD nm _ _) = n == nm
      theClassD _ = False
  in theOneOf "ClassD" n theClassD env

-- | Look up an InstD in the given Environment.
lookupInstD :: Env -> Class -> Failable Dec
lookupInstD env (Class n t) =
  let theInstD :: Dec -> Bool
      theInstD (InstD _ (Class nm ts) _)
        = and $ (n == nm) : [isSubType p c | (p, c) <- zip ts t]
      theInstD _ = False
  in theOneOf "InstD" (pretty (Class n t)) theInstD env

-- | Given a VarE in an environment return the polymorphic type and value of
-- that variable as determined by the environment. For methods, the type
-- returned is that of the class definition, not for any specific instance.
--
-- Fails if the variable could not be found in the environment.
lookupVar :: Env -> Sig -> Failable (Type, Exp)
lookupVar env s@(Sig n t) =
  case HT.lookup n (e_vitable env) of
     Just (DecVI (ValD (TopSig _ _ t) v)) -> return (t, v)
     Just (DecVI (PrimD {})) -> fail $ "lookupVar: " ++ n ++ " is primitive"
     Just (ClassVI cn cts st) ->
        let ts = assign (assignments st t) (map tyVarType cts)

            mlook :: Method -> Failable Exp
            mlook (Method nm body) | nm == n = return body
            mlook _ = fail "mlook"
        in do
            InstD _ (Class _ pts) ms <- lookupInstD env (Class cn ts)
            e <- msum (map mlook ms)
            let assigns = concat [assignments p c | (p, c) <- zip pts ts]
            return (st, assign assigns e)
     _ -> fail $ "lookupVar: " ++ n ++ " not found"

-- | Look up the value of a variable in an environment.
lookupVarValue :: Env -> Sig -> Failable Exp
lookupVarValue e s = fmap snd $ lookupVar e s

-- | Given a VarE in an environment, return the polymorphic type of that
-- variable as determined by the environment. For methods of classes, the
-- type returned is that in the class definition, not for any specific class
-- instance.
--
-- Fails if the variable could not be found in the environment.
lookupVarType :: Env -> Name -> Failable Type
lookupVarType env n = do
  case HT.lookup n (e_vitable env) of
    Just (DecVI (ValD (TopSig _ _ t) _)) -> return t
    Just (DecVI (PrimD (TopSig _ _ t))) -> return t
    Just (ClassVI _ _ t) -> return t
    Nothing -> fail $ "lookupVarType: '" ++ n ++ "' not found"

-- | Given the name of a method and a specific class instance for the method,
-- return the type of that method for the specific instance.
lookupMethodType :: Env -> Name -> Class -> Failable Type
lookupMethodType env n (Class _ ts) = do
    case HT.lookup n (e_vitable env) of
        Just (ClassVI _ vars t) ->
            return $ assign (zip (map tyVarName vars) ts) t
        _ -> fail $ "lookupMethodType: " ++ n ++ " not found"

-- | Given the name of a data constructor in the environment, return its type.
lookupDataConType :: Env -> Name -> Failable Type
lookupDataConType env n = 
    case HT.lookup n (e_dctable env) of
        Just t -> return t
        _ -> fail $ "lookupDataConType: " ++ n ++ " not found"

-- | Look up VarInfo for the variable with given signature.
-- Fails if the variable is not declared or an instance or primitive.
lookupVarInfo :: Env -> Sig -> Failable VarInfo
lookupVarInfo env (Sig n t) =
  case HT.lookup n (e_vitable env) of
     Just (DecVI (ValD {})) -> return Declared
     Just (DecVI (PrimD {})) -> return Primitive
     Just (ClassVI cn cts st) ->
        let ts = assign (assignments st t) (map tyVarType cts)
        in return $ Instance (Class cn ts)
     _ -> fail $ "lookupVarInfo: " ++ n ++ " not found"

