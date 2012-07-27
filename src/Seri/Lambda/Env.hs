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

import Data.Generics
import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types

-- | Env is an abstract data type representing information about a seri
-- environment.
newtype Env = Env [Dec]

-- | Build a seri environment from the given list of declarations.
mkEnv :: [Dec] -> Env
mkEnv = Env

-- | Make a small change to an environment.
-- Add the given declarations to the environment, overwriting any existing
-- conflicting ones.
tweak :: [Dec] -> Env -> Env
tweak tds (Env ds) = Env (tds ++ ds)

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
theOneOf kind n p (Env decs) =
    case filter p decs of
        [] -> fail $ kind ++ " for " ++ n ++ " not found"
        x -> return $ head x

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
lookupVar env@(Env decs) s@(Sig n t) =
  let failed = fail $ "Variable " ++ n ++ " not found"

      checkDec :: Dec -> Failable (Type, Exp)
      checkDec (ValD (TopSig nm _ t) v) | nm == n = return (t, v)
      checkDec (PrimD (TopSig nm _ _)) | nm == n
            = fail $ "lookupVar: " ++ n ++ " is primitive"
      checkDec (ClassD cn cts sigs) =
         case filter (\(TopSig sn _ _) -> sn == n) sigs of
            [] -> failed
            [TopSig _ _ st] -> 
                let ts = assign (assignments st t) (map tyVarType cts)

                    mlook :: Method -> Failable Exp
                    mlook (Method nm body) | nm == n = return body
                    mlook _ = fail "mlook"
                in do
                    InstD _ (Class _ pts) ms <- lookupInstD env (Class cn ts)
                    e <- msum (map mlook ms)
                    let assigns = concat [assignments p c | (p, c) <- zip pts ts]
                    return (st, assign assigns e)
      checkDec _ = failed
  in msum (map checkDec decs)

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
lookupVarType e@(Env decs) n  = do
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

              answer = listToMaybe (catMaybes (map getSig decs))
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

-- | Given the name of a data constructor in the environment, return its type.
lookupDataConType :: Env -> Name -> Failable Type
lookupDataConType (Env decs) n = 
    case catMaybes [typeofCon d n | d <- decs] of
        [] -> fail $ "data constructor " ++ n ++ " not found in env"
        [x] -> return x
        xs -> fail $ "multiple data constructors with name " ++ n ++ " found in env: " ++ pretty decs

-- | Look up VarInfo for the variable with given signature.
-- Fails if the variable is not declared or an instance or primitive.
lookupVarInfo :: Env -> Sig -> Failable VarInfo
lookupVarInfo (Env decs) (Sig n t) =
  let failed = fail $ "Variable " ++ n ++ " not found in environment"

      checkDec :: Dec -> Failable VarInfo
      checkDec (ValD (TopSig nm _ _) _) | nm == n = return Declared
      checkDec (PrimD (TopSig nm _ _)) | nm == n = return Primitive
      checkDec (ClassD cn cts sigs) =
         case filter (\(TopSig sn _ _) -> sn == n) sigs of
            [] -> failed
            [TopSig _ _ st] -> 
                let assigns = assignments st t
                    cts' = assign assigns (map tyVarType cts)
                in return $ Instance (Class cn cts')
      checkDec _ = failed
  in msum (map checkDec decs)

