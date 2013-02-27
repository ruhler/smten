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
{-# LANGUAGE PatternGuards #-}

module Smten.Typing.Check (TypeCheck(..)) where

import Data.List(nub)

import Smten.Failable
import Smten.Name
import Smten.Lit
import Smten.Sig
import Smten.Ppr
import Smten.Exp
import Smten.Dec
import Smten.Type

class TypeCheck a where
    -- | Type check the given object under the given environment.
    -- Fails if there is an error.
    typecheck :: Env -> a -> Failable ()

type TypeEnv = [(Name, Type)]

instance TypeCheck [Dec] where
    typecheck e = mapM_ (typecheck e)

instance TypeCheck Dec where
    typecheck env =
      let checkdec :: Dec -> Failable ()
          checkdec d@(ValD (TopSig n c t) e) =
            onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
              checkexp [] e
              if (typeof e /= t)
                then throw $ "checkdec: expecting type " ++ pretty t
                            ++ " but found type " ++ pretty (typeof e)
                            ++ " in expression " ++ pretty e
                else return ()
              instcheck env c e

          -- TODO: shouldn't we check that type signatures don't have any partially
          -- applied types?
          checkdec d@(DataD n vs cs) =
            onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $
               mapM_ (checkcon vs) cs
          checkdec (ClassD {}) = return ()

          checkdec d@(InstD ctx cls@(Class nm ts) ms) =
            let checkmeth :: Method -> Failable () 
                checkmeth m@(Method n b) =
                  onfail (\s -> throw $ s ++ "\n in method " ++ pretty n) $ do
                    checkexp [] b
                    texpected <- lookupMethodType env n cls
                    if typeof b /= texpected
                        then throw $ "checkmeth: expected type " ++ pretty texpected
                                ++ " but found type " ++ pretty (typeof b)
                                ++ " in Method " ++ pretty m
                        else return ()
                    -- TODO: use the context from the signature
                    instcheck env ctx b
    
                methdefined :: [Method] -> TopSig -> Bool 
                methdefined ms (TopSig n _ _) = n `elem` [mn | Method mn _ <- ms]
            in onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
                 mapM_ checkmeth ms
                 ClassD clsctx _ pts cms <- lookupClassD env nm 
                 case filter (not . methdefined ms) cms of
                    [] -> return ()
                    xs -> throw $ "methods not defined: " ++ show [pretty n | TopSig n _ _ <- xs]
                 let assigns = concat [assignments (tyVarType p) c | (p, c) <- zip pts ts]
                 mapM_ (satisfied env ctx) (assign assigns clsctx)
          checkdec d@(PrimD {}) = return ()

          checkcon :: [TyVar] -> Con -> Failable ()
          checkcon m (Con n ts) = mapM_ (checktype m) ts

          checktype :: [TyVar] -> Type -> Failable ()
          checktype m t
            | VarT n <- t
            , n `notElem` (map tyVarName m) = throw $ "type variable " ++ pretty n ++ " not in scope"
            | AppT a b <- t = checktype m a >> checktype m b
            | UnknownT <- t = throw $ "unknown type encountered"
            | otherwise = return ()

          -- checkexp tenv e
          -- Type check an expression.
          --    tenv - a mapping from bound variable name to type
          --    e - the expression to typecheck
          --  fails if expression does not type check.
          checkexp :: TypeEnv -> Exp -> Failable ()
          checkexp _ (LitE {}) = return ()

          checkexp _ c@(ConE s@(Sig n ct)) = do
             texpected <- lookupDataConType env n
             if isSubType texpected ct
                then return ()
                else throw $ "checkexp: expecting type " ++ pretty texpected ++ ", but found type " ++ pretty ct ++ " in data constructor " ++ pretty n

          checkexp tenv (VarE (Sig n t)) =
             case lookup n tenv of
                 Just t' | t == t' -> return ()
                 Just t' -> throw $ "expected variable of type:\n  " ++ pretty t'
                            ++ "\nbut " ++ pretty n ++ " has type:\n  " ++ pretty t
                 Nothing -> do
                     texpected <- lookupVarType env n
                     if isSubType texpected t
                         then return ()
                         else throw $ "expected variable of type:\n  " ++ pretty texpected
                                    ++ "\nbut " ++ pretty n ++ " has type:\n  " ++ pretty t

          checkexp tenv (AppE f x) = do    
             checkexp tenv f
             checkexp tenv x
             case typeof f of
                (AppT (AppT (ConT n) a) _) | n == name "->" ->
                    if a == typeof x
                        then return ()
                        else throw $ "checkexp app: expected type " ++ pretty a ++
                            " but got type " ++ pretty (typeof x) ++
                            " in expression " ++ pretty x
                t -> throw $ "expected function type, but got type " ++ pretty t ++ " in expression " ++ pretty f

          checkexp tenv (LamE (Sig n t) x) = checkexp ((n, t):tenv) x

          checkexp tenv (CaseE x k y n) = do
            checkexp tenv x
            checkexp tenv y
            checkexp tenv n

            -- Verify the argument type matches the type of the constructor.
            let at = last $ de_arrowsT (typeof k)
            if at == typeof x
                then return ()
                else throw $ "checkexp case: expected argument type " ++ pretty at ++
                        " but got type " ++ pretty (typeof x) ++
                        " in expression " ++ pretty x

            -- Verify y has the right type.
            let yt = arrowsT (init (de_arrowsT (typeof k)) ++ [typeof n])
            if yt == typeof y
                then return ()
                else throw $ "checkexp case: expected type " ++ pretty yt ++
                        " but got type " ++ pretty (typeof y) ++
                        " in expression " ++ pretty y
      in checkdec

-- Assert the given class requirement is satisfied.
satisfied :: Env -> Context -> Class -> Failable ()
satisfied e c cls = do
    let -- Get the immediate context implied by the given class.
        -- For example, getclassctx (Ord Foo) would return [Eq Foo].
        getclassctx :: Class -> Failable Context
        getclassctx (Class nm ts) = do
            ClassD ctx _ pts _ <- lookupClassD e nm
            let assigns = concat [assignments (tyVarType p) c | (p, c) <- zip pts ts]
            return (assign assigns ctx)

        -- expand a context by including all classes implied by it.
        expand :: Context -> Context -> Failable Context
        expand done [] = return done
        expand done todo = do
            let todo' = filter (flip notElem done) todo
            immediates <- mapM getclassctx todo'
            expand (done ++ todo) (concat immediates)
    fullc <- expand [] c
    let sat :: Class -> Failable ()
        sat cls | cls `elem` fullc = return ()
        sat cls@(Class _ ts) = do
            InstD ctx (Class _ pts) _ <- lookupInstD e cls
            let assigns = concat [assignments p c | (p, c) <- zip pts ts]
            mapM_ sat (assign assigns ctx)
    sat cls

-- | Verify all the needed class instances are either in the context or
-- declared for the given expression.
instcheck :: Env -> Context -> Exp -> Failable ()
instcheck env c e = do
    let check :: Sig -> Failable ()
        check s = do
            ctx <- lookupVarContext env s
            mapM_ (satisfied env c) ctx
    mapM_ check (free e)

