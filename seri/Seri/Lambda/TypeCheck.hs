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

module Seri.Lambda.TypeCheck (TypeCheck(..)) where

import Data.List(nub)

import Seri.Failable
import Seri.Lambda.Env
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types
import Seri.Lambda.Utils

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
                then throw $ "checkdec: expecting type " ++ pretty t ++ " in expression "
                            ++ pretty e ++ " but found type " ++ pretty (typeof e)
                else return ()
              instcheck env c e

          -- TODO: shouldn't we check the type signatures don't have any partially
          -- applied types?
          checkdec (DataD {}) = return ()
          checkdec (ClassD {}) = return ()

          checkdec d@(InstD ctx cls ms) =
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
            in onfail (\s -> throw $ s ++ "\n in declaration " ++ pretty d) $ do
                 mapM_ checkmeth ms
          checkdec d@(PrimD {}) = return ()

          checkpat :: Pat -> Failable [(Name, Type)]
          checkpat p@(ConP pt n ps) = do
             let ct = arrowsT ((map typeof ps) ++ [pt])
             texpected <- lookupDataConType env n
             if isSubType texpected ct
                then return ()
                else throw $ "checkpat: expecting type " ++ pretty texpected ++ ", but found type " ++ pretty ct
             binds <- mapM checkpat ps
             let concated = concat binds
             if length concated /= length (nub (map fst concated))
                then throw $ "VarP appears multiple times in " ++ pretty p
                else return ()
             let twants = init (unarrowsT ct)
             let assertpat w p =
                    if w == typeof p
                        then return () 
                        else throw $ "checkpat: expected type " ++ pretty w ++ " but found type " ++ pretty (typeof p) ++ " in pattern " ++ pretty p
             sequence [assertpat w p | (w, p) <- zip twants ps]
             return concated
          checkpat (VarP (Sig n t)) = return [(n, t)]
          checkpat (LitP i) = return []
          checkpat (WildP t) = return []
                

          checkmatch :: TypeEnv -> Match -> Failable ()
          checkmatch tenv (Match ps b) = do
            bindings <- mapM checkpat ps
            checkexp (concat bindings ++ tenv) b

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

          checkexp tenv (AppE f [x]) = do    
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
          checkexp tenv (AppE f (x:xs)) = checkexp tenv (AppE (AppE f [x]) xs)

          checkexp tenv (LaceE []) = throw "Empty lace expression"
          checkexp tenv (LaceE ms@(Match ps1 _ : _)) = do
             mapM_ (checkmatch tenv) ms
             let pattypes = map typeof ps1
             let badpattypes = filter (\ps -> pattypes /= map typeof ps) [ps | Match ps _ <- tail ms]
             if null badpattypes
                then return ()
                else
                  let printtypes ts = render (sep $ punctuate comma (map ppr ts))
                  in throw $ "Expected types " ++ printtypes pattypes
                            ++ " in pattern " ++ pretty (head (badpattypes))
                            ++ " but found types " ++ printtypes (head (badpattypes))
             let badmtypes = filter (\e -> typeof e /= typeof (head ms)) [e | Match _ e <- ms]
             if null badmtypes
                then return ()
                else throw $ "Expected type " ++ pretty (typeof (head ms))
                            ++ " in match expression " ++ pretty (head (badmtypes))
                            ++ " but found type " ++ pretty (typeof (head (badmtypes)))
          checkexp tenv e = error $ "checkexp: " ++ show e

      in checkdec

-- | Verify all the needed class instances are either in the context or
-- declared for the given expression.
instcheck :: Env -> Context -> Exp -> Failable ()
instcheck env c e = 
    let satisfied :: Class -> Failable ()
        satisfied cls | cls `elem` c = return ()
        satisfied cls@(Class _ ts) = do
            InstD ctx (Class _ pts) _ <- lookupInstD env cls
            let assigns = concat [assignments p c | (p, c) <- zip pts ts]
            mapM_ satisfied (assign assigns ctx)

        check :: Sig -> Failable ()
        check s = do
            ctx <- lookupVarContext env s
            mapM_ satisfied ctx
    in mapM_ check (free e)

