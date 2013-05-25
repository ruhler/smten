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
import Smten.Location
import Smten.Name
import Smten.Lit
import Smten.Sig
import Smten.Ppr
import Smten.Exp
import Smten.Dec
import Smten.Module
import Smten.Type


type TypeEnv = [(Name, Type)]

data TCS = TCS {
    tcs_env :: Env,
    tcs_tyvars :: [TyVar],
    tcs_vars :: TypeEnv,
    tcs_ctx :: Context,
    tcs_loc :: Location
}

type TC = ReaderT TCS Failable

instance MonadErrorSL TC where
    errloc = asks tcs_loc

withloc :: Location -> TC a -> TC a
withloc l = local (\r -> r { tcs_loc = l } )

-- wrongtype kind object expected found
-- Reports an error message of the form:
--  expecting type: 
--      <expected>
--  but found type:
--      <found>
--  in <kind>: <obj>
wrongtype :: (Ppr a) => String -> a -> Type -> Type -> TC b
wrongtype kind obj exp fnd
 = lthrow $ "expecting type:\n  " ++ pretty exp
       ++ "\nbut found type:\n  " ++ pretty fnd
       ++ "\nin " ++ kind ++ ": " ++ pretty obj
  

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
                then lthrow $ "type variable " ++ pretty n ++ " not in scope"
                else return ()
      | AppT a b <- t = typecheckM a >> typecheckM b
      | NumT {} <- t = return ()
      | OpT _ a b <- t = typecheckM a >> typecheckM b
      | UnknownT <- t = lthrow $ "unknown type encountered"

instance TypeCheck Sig where
    typecheckM (Sig n t) = typecheckM t

addVarTs :: VarTs a => a -> TCS -> TCS
addVarTs x tcs = tcs { tcs_tyvars = [TyVar n k | (n, k) <- varTs x]  ++ tcs_tyvars tcs }

addCtx :: Context -> TCS -> TCS
addCtx x tcs = tcs { tcs_ctx = x ++ (tcs_ctx tcs) }

instance TypeCheck TopSig where
    -- TODO: check the context too?
    typecheckM (TopSig n c t) = local (addCtx c . addVarTs t) $ typecheckM t

instance TypeCheck Exp where
   typecheckM (LitE {}) = return ()

   typecheckM c@(ConE l s@(Sig n ct)) = withloc l $ do
      typecheckM ct
      env <- asks tcs_env
      texpected <- lookupDataConType env n
      if isSubType texpected ct
         then return ()
         else wrongtype "data constructor" n texpected ct

   typecheckM (VarE l s@(Sig n t)) = withloc l $ do
      typecheckM t
      tenv <- asks tcs_vars
      case lookup n tenv of
          Just t' | eqtypes t t' -> return ()
          Just t' -> wrongtype "variable" n t' t
          Nothing -> do
              env <- asks tcs_env

              -- Verify the type is satisfied for this variable.
              texpected <- lookupVarType env n
              if isSubType texpected t
                  then return ()
                  else wrongtype "variable" n texpected t

              -- Verify the context is satisfied for this variable.
              vctx <- lookupVarContext env s
              mapM_ satisfied vctx

   typecheckM (AppE l f x) = withloc l $ do    
      typecheckM f
      typecheckM x
      case typeof f of
         t | Just (a, _) <- de_arrowT t ->
             if eqtypes a (typeof x)
                 then return ()
                 else wrongtype "expression" x a (typeof x)
           | otherwise -> wrongtype "expression" f (arrowT UnknownT UnknownT) t

   typecheckM (LamE l (Sig n t) x) = withloc l $ do
     local (\tcs -> tcs { tcs_vars = (n, t) : tcs_vars tcs}) $ typecheckM x

   typecheckM (CaseE l x k y n) = withloc l $ do
     typecheckM x
     typecheckM k
     typecheckM y
     typecheckM n

     -- Verify the argument type matches the type of the constructor.
     let at = last $ de_arrowsT (typeof k)
     if eqtypes at (typeof x)
         then return ()
         else wrongtype "expression" x at (typeof x)

     -- Verify y has the right type.
     let yt = arrowsT (init (de_arrowsT (typeof k)) ++ [typeof n])
     if eqtypes yt (typeof y)
         then return ()
         else wrongtype "expression" y yt (typeof y)

   typecheckM (LetE l bs x) = withloc l $ do
      local (\tcs -> tcs { tcs_vars = [(n, t) | Sig n t <- map fst bs] ++ tcs_vars tcs}) $ do
        let f :: (Sig, Exp) -> TC ()
            f (Sig n t, v) = do
                typecheckM v
                if eqtypes t (typeof v)
                    then return ()
                    else wrongtype "let expr" v t (typeof v)
        mapM f bs
        typecheckM x
    
        
instance TypeCheck TopExp where
    typecheckM (TopExp ts@(TopSig n c t) e) = do
        typecheckM ts
        local (addCtx c . addVarTs ts) $ typecheckM e
        if (typeof e /= t)
          then wrongtype "expression" e t (typeof e)
          else return ()

instance TypeCheck Module where
    typecheckM m = mapM_ typecheckM (mod_decs m)

instance TypeCheck Dec where
    typecheckM d@(ValD l e) = withloc l $ typecheckM e

    typecheckM d@(DataD l n vs cs) = withloc l $
         local (\tcs -> tcs { tcs_tyvars = vs }) (mapM_ typecheckM cs)

    typecheckM d@(ClassD l ctx nm vars ms) = withloc l $ do
      let checkmeth m@(TopExp (TopSig n c texpected) b) = do
             env <- asks tcs_env
             local (addCtx c . addVarTs texpected) $ typecheckM b
             if typeof b /= texpected
                 then wrongtype ("method " ++ pretty n) m texpected (typeof b)
                 else return ()
          me = Class nm (map tyVarType vars)
      local (addCtx (me : ctx) . addVarTs vars) $ mapM_ checkmeth ms

    typecheckM d@(InstD l ctx cls@(Class nm ts) ms) = withloc l $ do
      let checkmeth m@(Method n b) = do
             env <- asks tcs_env
             texpected <- lookupMethodType env n cls
             mctx <- lookupMethodContext env n cls
             local (addCtx mctx . addVarTs texpected) $ typecheckM b
             if typeof b /= texpected
                 then wrongtype ("method " ++ pretty n) m texpected (typeof b)
                 else return ()
      env <- asks tcs_env
      ClassD _ clsctx _ pts _ <- lookupClassD env nm 
      let assigns = concat [assignments (tyVarType p) c | (p, c) <- zip pts ts]
      local (addCtx ctx . addVarTs ts) $ do
           mapM_ checkmeth ms
           mapM_ satisfied (assign assigns clsctx)

    typecheckM d@(PrimD l n ts) = withloc l $ typecheckM ts

-- Assert the given class requirement is satisfied.
satisfied :: Class -> TC ()
satisfied cls = do
    e <- asks tcs_env
    c <- asks tcs_ctx
    let -- Get the immediate context implied by the given class.
        -- For example, getclassctx (Ord Foo) would return [Eq Foo].
        getclassctx (Class nm ts) = do
            ClassD _ ctx _ pts _ <- lookupClassD e nm
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
            InstD _ ctx (Class _ pts) _ <- lookupInstD e cls
            let assigns = concat [assignments p c | (p, c) <- zip pts ts]
            mapM_ sat (assign assigns ctx)
    sat cls

typecheck :: [Module] -> Failable ()
typecheck ms = {-# SCC "TypeCheck" #-}
  runReaderT (mapM_ typecheckM ms) (TCS (environ ms) [] [] [] lunknown)

eqtypes :: Type -> Type -> Bool
eqtypes a b = canonical a == canonical b

