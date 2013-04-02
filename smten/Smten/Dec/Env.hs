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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Hopefully efficient queries regarding a smten environment.
module Smten.Dec.Env (
    Env(), mkEnv, tweak,
    VarInfo(..),
    lookupVarType, lookupVarValue, lookupVar, lookupVarInfo, lookupVarContext,
    lookupMethodType, lookupMethodContext,
    lookupDataD, lookupDataConType,
    lookupInstD, lookupPrimD,
    lookupValD, lookupClassD,
    lookupTypeD,
    getDecls,
    ) where

import Debug.Trace

import Control.Monad
import Control.Monad.Error

import Data.Functor
import Data.List(partition)
import Data.Maybe
import qualified Data.Map as Map

import Smten.Name
import Smten.Failable
import Smten.HashTable as HT
import Smten.Sig
import Smten.Exp
import Smten.Dec.Dec
import Smten.Dec.Ppr
import Smten.Dec.Utils
import Smten.Ppr
import Smten.Type

-- | Env is an abstract data type representing information about a smten
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

 -- | The value belongs to the given class and has given type.
 -- Gives also the list of implementations for each instance of the class, and
 -- the default instance.
 -- The context is the context for the specific method, it does not include
 -- the class itself.
 | ClassVI Name [TyVar] Context Type Exp [([Type], Exp)]
    deriving (Eq, Show)

-- | Build a smten environment from the given list of declarations.
mkEnv :: [Dec] -> Env
mkEnv decs = Env decs (table (vitable decs)) (table (dctable decs))

vitable :: [Dec] -> [(Name, ValInfo)]
vitable decs =
  let isinst :: Dec -> Bool
      isinst (InstD {}) = True
      isinst _ = False

      (insts, notinsts) = partition isinst decs

      mkinst :: Dec -> Map.Map Name [([Type], Exp)]
      mkinst (InstD _ _ (Class _ tys) ms)
        = Map.fromList [(n, [(tys, e)]) | Method n e <- ms]
      mkinst _ = Map.empty

      methods :: Map.Map Name [([Type], Exp)]
      methods = Map.unionsWith (++) (map mkinst insts)

      videc :: Dec -> [(Name, ValInfo)]
      videc d@(ValD _ (TopExp (TopSig n _ _) _)) = [(n, DecVI d)]
      videc d@(PrimD _ (TopSig n _ _)) = [(n, DecVI d)]
      videc (ClassD _ _ cn ts sigs) =  
        let iexp :: TopExp -> (Name, ValInfo)
            iexp (TopExp (TopSig n ctx t) e) = (n, ClassVI cn ts ctx t e (fromMaybe [] (Map.lookup n methods)))
        in map iexp sigs
      videc d@(DataD _ n _ _) = [(n, DecVI d)]
      videc (InstD {}) = []
  in concat $ map videc notinsts

dctable :: [Dec] -> [(Name, Type)]
dctable decs =
  let dcdec :: Dec -> [(Name, Type)]
      dcdec d@(DataD _ dn vars cs) =
        let dccon :: Con -> (Name, Type)
            dccon (Con n ts) = (n, arrowsT (ts ++ [appsT (conT dn) (map tyVarType vars)]))
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
theOneOf :: (MonadError String m) => String -> String -> (Dec -> Bool) -> Env -> m Dec
theOneOf kind n p e =
    case filter p (e_decls e) of
        [] -> throw $ kind ++ " for " ++ n ++ " not found"
        x -> return $ head x

-- | Look up a ValD with given Name in the given Environment.
lookupValD :: Env -> Name -> Failable Dec
lookupValD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(ValD {})) -> return d
     _ -> throw $ "lookupValD: " ++ pretty n ++ " is not a ValD"

-- | Look up a PrimD with given Name in the given Environment.
lookupPrimD :: Env -> Name -> Failable Dec
lookupPrimD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(PrimD {})) -> return d
     _ -> throw $ "lookupPrimD: " ++ pretty n ++ " is not a PrimD"
 
        
-- | Look up a DataD with given type constructor Name in the given
-- Environment.
lookupDataD :: (MonadError String m) => Env -> Name -> m Dec
lookupDataD env n =
  case (HT.lookup n (e_vitable env)) of
     Just (DecVI d@(DataD {})) -> return d  
     _ -> throw $ "lookupDataD: " ++ pretty n ++ " is not a DataD"

-- | Look up a ClassD with given Name in the given Environment.
lookupClassD :: (MonadError String m) => Env -> Name -> m Dec
lookupClassD env n =
  let theClassD :: Dec -> Bool
      theClassD (ClassD _ _ nm _ _) = n == nm
      theClassD _ = False
  in theOneOf "ClassD" (pretty n) theClassD env

-- | Look up the DataD or ClassD associated with the given Name in the given
-- Environment.
lookupTypeD :: (MonadPlus m, MonadError String m) => Env -> Name -> m Dec
lookupTypeD e n = lookupDataD e n `mplus` lookupClassD e n

-- | Look up an InstD in the given Environment.
lookupInstD :: (MonadError String m) => Env -> Class -> m Dec
lookupInstD env (Class n t) =
  let theInstD :: Dec -> Bool
      theInstD (InstD _ _ (Class nm ts) _)
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
     Just (DecVI (ValD _ (TopExp (TopSig _ _ t) v))) -> return (t, v)
     Just (DecVI (PrimD {})) -> throw $ "lookupVar: " ++ pretty n ++ " is primitive"
     Just (ClassVI cn cts _ st def meths) -> return $ 
        let ts = assign (assignments st t) (map tyVarType cts)

            theMeth :: ([Type], Exp) -> Bool
            theMeth (x, _) = and [isSubType p c | (p, c) <- zip x ts]

            (assigns, e) = case filter theMeth meths of
                            [] -> (assignments st (typeof def), def)
                            [x] -> (concat [assignments p c | (p, c) <- zip (fst x) ts], snd x)
                            xs -> -- multiple instances should not happen
                                  -- it means we have a bug in the type checker
                                  error $ "INTERNAL SMTEN ERROR: overlapping instances for: " ++ pretty s
        in (st, assign assigns e)
     _ -> throw $ "lookupVar: " ++ pretty n ++ " not found"

-- | Look up the value of a variable in an environment.
lookupVarValue :: Env -> Sig -> Failable Exp
lookupVarValue e s = snd <$> lookupVar e s

-- | Given a VarE in an environment, return the polymorphic type of that
-- variable as determined by the environment. For methods of classes, the
-- type returned is that in the class definition, not for any specific class
-- instance.
--
-- Fails if the variable could not be found in the environment.
lookupVarType :: (MonadError String m) => Env -> Name -> m Type
lookupVarType env n = do
  case HT.lookup n (e_vitable env) of
    Just (DecVI (ValD _ (TopExp (TopSig _ _ t) _))) -> return t
    Just (DecVI (PrimD _ (TopSig _ _ t))) -> return t
    Just (ClassVI _ _ _ t _ _) -> return t
    Nothing -> throw $ "lookupVarType: '" ++ pretty n ++ "' not found"

-- | Given the name of a method and a specific class instance for the method,
-- return the type of that method for the specific instance.
lookupMethodType :: (MonadError String m) => Env -> Name -> Class -> m Type
lookupMethodType env n (Class _ ts) = do
    case HT.lookup n (e_vitable env) of
        Just (ClassVI _ vars _ t _ _) ->
            return $ assign (zip (map tyVarName vars) ts) t
        _ -> throw $ "lookupMethodType: " ++ pretty n ++ " not found"

-- | Given the name of a method and a specific class instance for the method,
-- return the context of that method for the specific instance.
lookupMethodContext :: (MonadError String m) => Env -> Name -> Class -> m Context
lookupMethodContext env n (Class _ ts) = do
    case HT.lookup n (e_vitable env) of
        Just (ClassVI _ vars ctx _ _ _) ->
            return $ assign (zip (map tyVarName vars) ts) ctx
        _ -> throw $ "lookupMethodContext: " ++ pretty n ++ " not found"

-- | Given the name of a data constructor in the environment, return its type.
lookupDataConType :: (MonadError String m) => Env -> Name -> m Type
lookupDataConType env n = 
    case HT.lookup n (e_dctable env) of
        Just t -> return t
        _ -> throw $ "lookupDataConType: " ++ pretty n ++ " not found"

-- | Look up VarInfo for the variable with given signature.
-- Fails if the variable is not declared or an instance or primitive.
lookupVarInfo :: Env -> Sig -> Failable VarInfo
lookupVarInfo env (Sig n t) =
  case HT.lookup n (e_vitable env) of
     Just (DecVI (ValD {})) -> return Declared
     Just (DecVI (PrimD {})) -> return Primitive
     Just (ClassVI cn cts _ st _ _) ->
        let ts = assign (assignments st t) (map tyVarType cts)
        in return $ Instance (Class cn ts)
     _ -> throw $ "lookupVarInfo: " ++ pretty n ++ " not found"

-- | Look up the context specified for the given variable with given
-- signature.
-- Fails if the variable is not declared.
lookupVarContext :: (MonadError String m) => Env -> Sig -> m Context
lookupVarContext env (Sig n t) = 
  case HT.lookup n (e_vitable env) of
     Just (DecVI (ValD _ (TopExp (TopSig _ ctx st) _))) ->
        return $ assign (assignments st t) ctx
     Just (DecVI (PrimD _ (TopSig _ ctx st))) ->
        return $ assign (assignments st t) ctx
     Just (ClassVI cn cts ctx st _ _) ->
        return $ assign (assignments st t) (Class cn (map tyVarType cts) : ctx)
     _ -> throw $ "lookupVarContext: " ++ pretty n ++ " not found"

instance Ppr Env where
   ppr e = ppr $ e_decls e

getDecls :: Env -> [Dec]
getDecls = e_decls

