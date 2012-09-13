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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Seri.Lambda.Modularity (
    Module(..), Import(..), flatten, flatten1,
    ) where

import Control.Monad.State

import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Prelude
import Seri.Lambda.Types
import Seri.Failable

-- | Currently imports are restricted to the form:
-- > import Foo.Bar
-- No hiding or qualification is supported.
data Import = Import Name
    deriving(Show, Eq)

data Module = Module Name [Import] [Dec]
    deriving(Show, Eq)

instance Ppr Import where
    ppr (Import n) = text "import" <+> ppr n <> semi

instance Ppr Module where
    ppr (Module n imps decs)
        = text "module" <+> ppr n <+> text "where" <+> text "{"
            $+$ nest tabwidth (vcat (map ppr imps) $+$ ppr decs) $+$ text "}"

lookupModule :: Name -> [Module] -> Failable Module
lookupModule n [] = fail $ "module " ++ pretty n ++ " not found"
lookupModule n (m@(Module nm _ _) : _) | (n == nm) = return m
lookupModule n (_:ms) = lookupModule n ms

data QS = QS {
    qs_env :: [Module],     -- ^ The environment
    qs_me :: Module,        -- ^ The current module
    qs_bound :: [Name]      -- ^ List of bound variable names
}

type QualifyM = StateT QS Failable

onfailq :: (String -> QualifyM a) -> QualifyM a -> QualifyM a
onfailq f q = do
   s <- get
   case (attempt $ runStateT q s) of
     Left msg -> f msg
     Right (v, s') -> put s' >> return v

mename :: QualifyM Name
mename = do
    Module n _ _ <- gets qs_me
    return n 

withbound :: [Name] -> QualifyM a -> QualifyM a
withbound binds x = do
    bound <- gets qs_bound
    modify $ \qs -> qs { qs_bound = (binds ++ bound) }
    r <- x
    modify $ \qs -> qs { qs_bound = bound }
    return r

isbound :: Name -> QualifyM Bool
isbound n = do
    bound <- gets qs_bound
    return $ n `elem` bound

class Qualify a where
    -- | Resolve all unqualified names in the given object.
    qualify :: a -> QualifyM a 

instance Qualify Module where
    qualify m@(Module nm is ds) = do
        modify $ \qs -> qs { qs_me = m }
        ds' <- mapM qualify ds
        modify $ \qs -> qs { qs_me = (error "not in module") }
        return (Module nm is ds')

instance Qualify TopSig where
    qualify (TopSig nm ctx t) = do
        menm <- mename
        let nm' = menm `nappend` name "." `nappend` nm
        ctx' <- qualify ctx
        t' <- qualify t
        return (TopSig nm' ctx' t')

instance Qualify Dec where
    qualify d@(ValD ts body) = 
        onfailq (\msg -> fail (msg ++ "\n when flattening " ++ pretty d)) $ do
           ts' <- qualify ts
           body' <- qualify body
           return (ValD ts' body')

    -- TODO: qualify type and data constructors.
    qualify d@(DataD {}) = return d

    -- TODO: qualify class names
    qualify (ClassD nm vars sigs) = do
        sigs' <- mapM qualify sigs
        return (ClassD nm vars sigs')

    -- TODO: qualify class name
    qualify (InstD ctx cls meths) = do
        meths' <- mapM qualify meths
        return (InstD ctx cls meths')

    qualify (PrimD ts) = do
        ts' <- qualify ts
        return (PrimD ts')

instance Qualify Context where
    -- TODO: qualify context
    qualify ctx = return ctx

instance Qualify Type where
    -- TODO: qualify type
    qualify ty = return ty

instance Qualify Exp where
    qualify e@(LitE {}) = return e
    qualify (CaseE e ms) = do
        e' <- qualify e
        ms' <- mapM qualify ms
        return (CaseE e' ms')
    qualify (AppE a b) = do
        a' <- qualify a
        b' <- qualify b
        return (AppE a' b')
    qualify (LamE (Sig n t) b) = do
        t' <- qualify t
        b' <- withbound [n] (qualify b)
        return (LamE (Sig n t') b')

    -- TODO: qualify data constructors
    qualify e@(ConE {}) = return e

    qualify (VarE (Sig n t)) = do
        t' <- qualify t
        bound <- isbound n
        if bound 
            then return (VarE (Sig n t'))
            else do
                n' <- resolve n
                return (VarE (Sig n' t'))

instance Qualify Match where
    qualify (Match p m) = do
        p' <- qualify p
        m' <- withbound (bindingsP' p) $ qualify m 
        return (Match p' m')

instance Qualify Pat where
    -- TODO: qualify patterns
    qualify p = return p

instance Qualify Method where
    qualify (Method nm e) = do
        e' <- qualify e
        nm' <- resolve nm
        return (Method nm' e')
        
-- | Return the unique name for the entity referred to by the given name.
resolve :: Name -> QualifyM Name
resolve n =
  let hasName :: Name -> Dec -> Bool
      hasName n (ValD (TopSig nm _ _) _) = (n == nm)
      hasName n (DataD nm _ _) = (n == nm)
      hasName n (ClassD nm _ sigs) =
        let hasns [] = False
            hasns ((TopSig snm _ _):_) | n == snm = True
            hasns (_:ss) = hasns ss
        in (n == nm) || hasns sigs
      hasName n (InstD {}) = False
      hasName n (PrimD (TopSig nm _ _)) = (n == nm)

      r :: [Module] -> Module -> Failable Name
      r env me@(Module menm imports _) = 
        let immediate :: Module -> [Name]
            immediate (Module mnm _ ds) = map (const (mnm `nappend` name "." `nappend` n)) (filter (hasName n) ds)
        in do
            imported <- mapM (\(Import mn) -> lookupModule mn env) imports
            let names = map immediate (me : imported)
            case concat names of
                [] -> fail $ "'" ++ pretty n ++ "' not found in module " ++ pretty menm
                [x] -> return x
                xs -> fail $ "'" ++ pretty n ++ "' is ambiguious: " ++ show xs
  in do
      me <- gets qs_me
      env <- gets qs_env
      lift $ r env me

-- | Flatten a complete module hierarchy.
-- Includes the builtin prelude.
flatten :: [Module] -> Failable [Dec]
flatten ms = do
    ds <- mapM (flatten1 ms) ms
    return $ concat (prelude:ds)

-- | Flatten a single module.
flatten1 :: [Module]    -- ^ The environment
            -> Module   -- ^ The module to flatten
            -> Failable [Dec] -- ^ Flattened declarations from the module
flatten1 ms m = do
  (Module _ _ d, _) <- runStateT (qualify m) (QS ms (error "not in module") [])
  return d
            

