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

-- | Definitions and utilities for working with seri environments. An
-- environment captures an object in the context of a bunch of seri
-- declarations.
module Seri.Lambda.Env (
    Env,
    VarInfo(..),
    minimize, sort,
    lookupVarType, lookupVarValue, lookupVar, lookupVarInfo,
    lookupMethodType,
    lookupDataD, lookupDataConType,
    lookupInstD, lookupPrimD,
    ) where

import Debug.Trace

import Data.Generics
import Data.List(nub, partition)
import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types

type Env = [Dec]

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
theOneOf kind n p env =
    case filter p env of
        [x] -> return x
        [] -> fail $ kind ++ " for " ++ n ++ " not found"
        xs -> fail $ "Multiple definitions for " ++ n ++ " found: " ++ concatMap pretty xs


-- | Look up a ValD with given Name in the given Environment.
lookupValD :: Env -> Name -> Failable Dec
lookupValD env n =
  let theValD :: Dec -> Bool
      theValD (ValD (TopSig nm _ _) _) = n == nm
      theValD _ = False
  in theOneOf "ValD" n theValD env

-- | Look up a PrimD with given Name in the given Environment.
lookupPrimD :: Env -> Name -> Failable Dec
lookupPrimD env n =
  let thePrimD :: Dec -> Bool
      thePrimD (PrimD (TopSig nm _ _)) = n == nm
      thePrimD _ = False
  in theOneOf "PrimD" n thePrimD env
 
        
-- | Look up a DataD with given type constructor Name in the given
-- Environment.
lookupDataD :: Env -> Name -> Failable Dec
lookupDataD env n =
  let theDataD :: Dec -> Bool
      theDataD (DataD nm _ _) = n == nm
      theDataD _ = False
  in theOneOf "DataD" n theDataD env

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

-- | Look up the type of a method in the given class.
lookupSig :: Env -> Name -> Name -> Failable Type
lookupSig env cls meth =
  let sigInClass :: [TopSig] -> Failable Type
      sigInClass [] = fail $ "method " ++ meth ++ " not found in class " ++ cls
      sigInClass ((TopSig n _ t):_) | n == meth = return t
      sigInClass (_:xs) = sigInClass xs
  in do
     ClassD _ _ sigs <- lookupClassD env cls
     sigInClass sigs


-- | Given a VarE in an environment return the polymorphic type and value of
-- that variable as determined by the environment. For methods, the type
-- returned is that of the class definition, not for any specific instance.
--
-- Fails if the variable could not be found in the environment.
lookupVar :: Env -> Sig -> Failable (Type, Exp)
lookupVar env s@(Sig n _) = do
    vi <- lookupVarInfo env s
    case vi of
        Primitive -> fail $ "lookupVar: " ++ n ++ " is primitive"
        Declared -> do
            (ValD (TopSig _ _ t) v) <- lookupValD env n
            return (t, v)
        (Instance cls@(Class cn ts)) ->
            let mlook :: [Method] -> Failable (Type, Exp)
                mlook [] = fail $ "method " ++ n ++ " not found"
                mlook ((Method nm body):ms) | nm == n = do
                    t <- lookupSig env cn n
                    return (t, body)
                mlook (m:ms) = mlook ms
            in do
                InstD _ (Class _ pts) ms <- lookupInstD env cls
                (t, e) <- mlook ms
                let assigns = concat [assignments p c | (p, c) <- zip pts ts]
                return (t, assign assigns e)

-- | Look up the value of a variable in an environment.
lookupVarValue :: Env -> Sig -> Failable Exp
lookupVarValue e s = lookupVar e s >>= return . snd

-- | Given a VarE in an environment, return the polymorphic type of that
-- variable as determined by the environment. For methods of classes, the
-- type returned is that in the class definition, not for any specific class
-- instance.
--
-- Fails if the variable could not be found in the environment.
lookupVarType :: Env -> Name -> Failable Type
lookupVarType e n  = do
    case (attemptM $ lookupValD e n, attemptM $ lookupPrimD e n) of
       (Just (ValD (TopSig _ _ t) _), _) -> return t
       (_, Just (PrimD (TopSig _ _ t))) -> return t
       _ ->
          let getSig :: Dec -> Maybe Type
              getSig (ClassD cn _ sigs) =
                case filter (\(TopSig sn _ _) -> sn == n) sigs of
                    [] -> Nothing
                    [TopSig _ _ t] -> Just t
              getSig _ = Nothing

              answer = listToMaybe (catMaybes (map getSig e))
          in case answer of
                Just t -> return t
                Nothing -> fail $ "lookupVarType: '" ++ n ++ "' not declared"

-- | Given the name of a method and a specific class instance for the method,
-- return the type of that method for the specific instance.
lookupMethodType :: Env -> Name -> Class -> Failable Type
lookupMethodType e n (Class cn ts) = do
    t <- lookupVarType e n
    ClassD _ vars _ <- lookupClassD e cn
    return $ assign (zip (map tyVarName vars) ts) t

union :: (Eq a) => [a] -> [a] -> [a]
union a b = nub $ a ++ b

-- declarations env x
-- Return the set of declarations in the given environment a thing depends on.
declarations :: (Data a) => Env -> a -> Env
declarations env =
  let qexp :: Exp -> [Dec]
      qexp (VarE s@(Sig n _)) =
         case (attemptM $ lookupVarInfo env s) of
            Just Primitive -> attemptM $ lookupPrimD env n
            Just Declared -> attemptM $ lookupValD env n
            Just (Instance cls@(Class ni ts)) ->
                catMaybes [attemptM $ lookupClassD env ni,
                             attemptM $ lookupInstD env cls]
            _ -> []
      qexp e = []

      qtype :: Type -> [Dec]
      qtype (ConT n) = attemptM $ lookupDataD env n
      qtype t = []

      qclass :: Class -> [Dec]
      qclass cls@(Class n ts) = catMaybes [
            attemptM $ lookupClassD env n,
            attemptM $ lookupInstD env cls]

      query :: (Typeable a) => a -> [Dec]
      query = extQ (extQ (mkQ [] qexp) qtype) qclass
  in everything union query

-- | Minimize an environment.
-- Remove any declarations in the environment not needed by the object in the
-- environment.
minimize :: (Data a) => Env -> a -> Env
minimize m x =
  let alldecls :: [Dec] -> [Dec]
      alldecls d =
        let ds = declarations m d
            dds = d `union` ds
        in if (length d == length dds)
            then d
            else alldecls dds
  in alldecls (declarations m x)

-- | sort ds
-- Perform a topological sort of declarations ds by dependency.
--   returns (sorted, mutual)
--  sorted - the list of sorted declarations. Declarations earlier in the list
--           do not depend on declarations later in the list.
--  mutual - a list of the remaining mutually dependent declarations from ds.
sort :: [Dec] -> ([Dec], [Dec])
sort ds = 
  let dependencies :: [(Dec, [Dec])]
      dependencies = [(d, declarations ds d) | d <- ds]

      sorte :: [Dec] -> [(Dec, [Dec])] -> ([Dec], [Dec])
      sorte sorted unsorted = 
        let indep :: (Dec, [Dec]) -> Bool
            indep (d, deps) = and [dp `elem` sorted | dp <- deps]
        in case partition indep unsorted of
            ([], deps) -> (sorted, map fst unsorted)
            (indeps, deps) -> sorte (sorted ++ (map fst indeps)) deps
  in sorte [] dependencies

-- | Given the name of a data constructor in the environment, return its type.
lookupDataConType :: Env -> Name -> Failable Type
lookupDataConType decs n = 
    case catMaybes [typeofCon d n | d <- decs] of
        [] -> fail $ "data constructor " ++ n ++ " not found in env"
        [x] -> return x
        xs -> fail $ "multiple data constructors with name " ++ n ++ " found in env: " ++ pretty decs

-- | Look up VarInfo for the variable with given signature.
-- Fails if the variable is not declared or an instance or primitive.
lookupVarInfo :: Env -> Sig -> Failable VarInfo
lookupVarInfo env (Sig n t)
  = case (attemptM $ lookupValD env n, attemptM $ lookupPrimD env n) of
        (Just _, _) -> return Declared
        (_, Just _) -> return Primitive
        _ ->
          let getSig :: Dec -> Maybe (Name, [TyVar], Type)
              getSig (ClassD cn cts sigs) =
                case filter (\(TopSig sn _ _) -> sn == n) sigs of
                    [] -> Nothing
                    [TopSig _ _ t] -> Just (cn, cts, t)
              getSig _ = Nothing

              answer = listToMaybe (catMaybes (map getSig env))
          in case answer of 
              Nothing -> fail $ "Variable " ++ n ++ " not found in the environment"
              Just (cn, cts, st) ->
                 let assigns = assignments st t
                     cts' = assign assigns (map tyVarType cts)
                 in return $ Instance (Class cn cts')

