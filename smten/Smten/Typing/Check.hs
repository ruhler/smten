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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module Smten.Typing.Check (typecheck) where

import Control.Monad.Error
import Control.Monad.Reader

import Smten.Failable
import Smten.Name
import Smten.Lit
import Smten.Sig
import Smten.Ppr
import Smten.Exp
import Smten.Dec
import Smten.Type


type TypeEnv = [(Name, Type)]

-- TODO: add Context to this environment, and use it for instcheck and
-- checkmeth.
data TCS = TCS {
    tcs_env :: Env,
    tcs_tyvars :: [TyVar],
    tcs_vars :: TypeEnv
}

type TC = ReaderT TCS Failable

class TypeCheck a where
    -- | Type check the given object.
    -- Fails if there is a type error.
    typecheckM :: a -> TC ()

instance TypeCheck [Dec] where
    typecheckM = mapM_ typecheckM

instance TypeCheck Con where
    typecheckM (Con n ts) = mapM_ typecheckM ts

instance TypeCheck Type where
    typecheckM t
      | ConT {} <- t = return ()
      | VarT n _ <- t = do
           m <- asks tcs_tyvars
           if n `notElem` (map tyVarName m)
                then throw $ "type variable " ++ pretty n ++ " not in scope"
                else return ()
      | AppT a b <- t = typecheckM a >> typecheckM b
      | NumT {} <- t = return ()
      | OpT _ a b <- t = typecheckM a >> typecheckM b
      | UnknownT <- t = throw $ "unknown type encountered"

instance TypeCheck Sig where
    typecheckM (Sig n t) = typecheckM t

addVarTs :: VarTs a => a -> TCS -> TCS
addVarTs x tcs = tcs { tcs_tyvars = [TyVar n k | (n, k) <- varTs x]  ++ tcs_tyvars tcs }

instance TypeCheck TopSig where
    -- TODO: check the context too?
    typecheckM (TopSig n c t) = local (addVarTs t) $ typecheckM t

instance TypeCheck Exp where
   typecheckM (LitE {}) = return ()

   typecheckM c@(ConE s@(Sig n ct)) = do
      typecheckM ct
      env <- asks tcs_env
      texpected <- lookupDataConType env n
      if isSubType texpected ct
         then return ()
         else throw $ "expecting type " ++ pretty texpected ++ ", but found type " ++ pretty ct ++ " in data constructor " ++ pretty n

   typecheckM (VarE (Sig n t)) = do
      typecheckM t
      tenv <- asks tcs_vars
      case lookup n tenv of
          Just t' | t == t' -> return ()
          Just t' -> throw $ "expected variable of type:\n  " ++ pretty t'
                     ++ "\nbut " ++ pretty n ++ " has type:\n  " ++ pretty t
          Nothing -> do
              env <- asks tcs_env
              texpected <- lookupVarType env n
              if isSubType texpected t
                  then return ()
                  else throw $ "expected variable of type:\n  " ++ pretty texpected
                             ++ "\nbut " ++ pretty n ++ " has type:\n  " ++ pretty t

   typecheckM (AppE f x) = do    
      typecheckM f
      typecheckM x
      case typeof f of
         (AppT (AppT (ConT n _) a) _) | n == name "->" ->
             if a == typeof x
                 then return ()
                 else throw $ "expected type " ++ pretty a ++
                     " but got type " ++ pretty (typeof x) ++
                     " in expression " ++ pretty x
         t -> throw $ "expected function type, but got type " ++ pretty t ++ " in expression " ++ pretty f

   typecheckM (LamE (Sig n t) x)
     = local (\tcs -> tcs { tcs_vars = (n, t) : tcs_vars tcs}) $ typecheckM x

   typecheckM (CaseE x k y n) = do
     typecheckM x
     typecheckM k
     typecheckM y
     typecheckM n

     -- Verify the argument type matches the type of the constructor.
     let at = last $ de_arrowsT (typeof k)
     if at == typeof x
         then return ()
         else throw $ "expected argument type " ++ pretty at ++
                 " but got type " ++ pretty (typeof x) ++
                 " in expression " ++ pretty x

     -- Verify y has the right type.
     let yt = arrowsT (init (de_arrowsT (typeof k)) ++ [typeof n])
     if yt == typeof y
         then return ()
         else throw $ "expected type " ++ pretty yt ++
                 " but got type " ++ pretty (typeof y) ++
                 " in expression " ++ pretty y

instance TypeCheck TopExp where
    typecheckM (TopExp ts@(TopSig n c t) e) = do
        typecheckM ts
        local (addVarTs ts) $ typecheckM e
        if (typeof e /= t)
          then throw $ "expecting type " ++ pretty t
                      ++ " but found type " ++ pretty (typeof e)
                      ++ " in expression " ++ pretty e
          else return ()
        env <- asks tcs_env
        instcheck env c e
        

instance TypeCheck Dec where
    typecheckM d@(ValD e) = 
      onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
        typecheckM e

    typecheckM d@(DataD n vs cs) =
      onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $
         local (\tcs -> tcs { tcs_tyvars = vs }) (mapM_ typecheckM cs)

    typecheckM (ClassD {}) = return ()

    typecheckM d@(InstD ctx cls@(Class nm ts) ms) =
      let checkmeth m@(Method n b) =
            onfail (\s -> throw $ s ++ "\n in method " ++ pretty n) $ do
              env <- asks tcs_env
              texpected <- lookupMethodType env n cls
              local (addVarTs texpected) $ typecheckM b
              if typeof b /= texpected
                  then throw $ "expected type " ++ pretty texpected
                          ++ " but found type " ++ pretty (typeof b)
                          ++ " in Method " ++ pretty m
                  else return ()
              -- TODO: use the context from the signature
              instcheck env ctx b
    
          methdefined :: [Method] -> TopSig -> Bool 
          methdefined ms (TopSig n _ _) = n `elem` [mn | Method mn _ <- ms]
      in onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
           local (addVarTs ts) $ mapM_ checkmeth ms
           env <- asks tcs_env
           ClassD clsctx _ pts cms <- lookupClassD env nm 
           case filter (not . methdefined ms) cms of
              [] -> return ()
              xs -> throw $ "methods not defined: " ++ show [pretty n | TopSig n _ _ <- xs]
           let assigns = concat [assignments (tyVarType p) c | (p, c) <- zip pts ts]
           mapM_ (satisfied env ctx) (assign assigns clsctx)
    typecheckM d@(PrimD ts) =
      onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
        typecheckM ts

-- Assert the given class requirement is satisfied.
satisfied :: (MonadError String m) => Env -> Context -> Class -> m ()
satisfied e c cls = do
    let -- Get the immediate context implied by the given class.
        -- For example, getclassctx (Ord Foo) would return [Eq Foo].
        getclassctx (Class nm ts) = do
            ClassD ctx _ pts _ <- lookupClassD e nm
            let assigns = concat [assignments (tyVarType p) c | (p, c) <- zip pts ts]
            return (assign assigns ctx)

        -- expand a context by including all classes implied by it.
        expand done [] = return done
        expand done todo = do
            let todo' = filter (flip notElem done) todo
            immediates <- mapM getclassctx todo'
            expand (done ++ todo) (concat immediates)
    fullc <- expand [] c
    let sat cls | cls `elem` fullc = return ()
        sat cls@(Class _ ts) = do
            InstD ctx (Class _ pts) _ <- lookupInstD e cls
            let assigns = concat [assignments p c | (p, c) <- zip pts ts]
            mapM_ sat (assign assigns ctx)
    sat cls

-- | Verify all the needed class instances are either in the context or
-- declared for the given expression.
--
-- TODO: incorporate this into the rest of the type checking traversal.
instcheck :: (MonadError String m) => Env -> Context -> Exp -> m ()
instcheck env c e = do
    let check :: (MonadError String m) => Sig -> m ()
        check s = do
            ctx <- lookupVarContext env s
            mapM_ (satisfied env c) ctx
    mapM_ check (free e)

typecheck :: (TypeCheck a) => Env -> a -> Failable ()
typecheck env x = runReaderT (typecheckM x) (TCS env [] [])

