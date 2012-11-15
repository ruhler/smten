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

module Seri.Module.Module (
    Module(..), Import(..), Synonym(..), flatten, flatten1,
    ) where

import Control.Monad.State
import Data.Functor
import Data.Maybe(fromMaybe)

import qualified Seri.HashTable as HT
import Seri.Failable
import Seri.Ppr
import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.Exp
import Seri.Dec


-- | Currently imports are restricted to the form:
-- > import Foo.Bar
-- No hiding or qualification is supported.
data Import = Import Name
    deriving(Show, Eq)

-- | Currently synonyms are restricted to have no arguments.
-- type Foo = ...
data Synonym = Synonym Name Type 
    deriving(Show, Eq)

data Module = Module Name [Import] [Synonym] [Dec]
    deriving(Show, Eq)

instance Ppr Import where
    ppr (Import n) = text "import" <+> ppr n <> semi

instance Ppr Synonym where
    ppr (Synonym n t) = text "type" <+> ppr n <+> text "=" <+> ppr t <> semi

instance Ppr Module where
    ppr (Module n imps syns decs)
        = text "module" <+> ppr n <+> text "where" <+> text "{"
            $+$ nest tabwidth (
                vcat (map ppr imps)
                $+$ vcat (map ppr syns)
                $+$ ppr decs) $+$ text "}"

lookupModule :: Name -> [Module] -> Failable Module
lookupModule n [] = throw $ "module " ++ pretty n ++ " not found"
lookupModule n (m@(Module nm _ _ _) : _) | (n == nm) = return m
lookupModule n (_:ms) = lookupModule n ms

data QS = QS {
    qs_env :: [Module],     -- ^ The environment
    qs_me :: Module,        -- ^ The current module
    qs_bound :: [Name],     -- ^ List of bound variable names
    qs_syns :: HT.HashTable Name Type -- ^ All type synonyms
}

type QualifyM = StateT QS Failable

mkSyns :: [Synonym] -> HT.HashTable Name Type
mkSyns xs = HT.table [(n, t) | Synonym n t <- xs]

onfailq :: (String -> QualifyM a) -> QualifyM a -> QualifyM a
onfailq f q = do
   s <- get
   case (attempt $ runStateT q s) of
     Left msg -> f msg
     Right (v, s') -> put s' >> return v

mename :: QualifyM Name
mename = do
    Module n _ _ _ <- gets qs_me
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
    qualify m@(Module nm is sy ds) = do
        modify $ \qs -> qs { qs_me = m }
        sy' <- mapM qualify sy
        ds' <- mapM qualify ds
        modify $ \qs -> qs { qs_me = (error "not in module") }
        return (Module nm is sy' ds')

instance Qualify Synonym where
    qualify (Synonym n t) = Synonym n <$> qualify t

instance Qualify TopSig where
    qualify (TopSig nm ctx t) = do
        menm <- mename
        let nm' = menm `nappend` name "." `nappend` nm
        ctx' <- qualify ctx
        t' <- qualify t
        return (TopSig nm' ctx' t')

instance (Qualify a) => Qualify [a] where
    qualify = mapM qualify

instance Qualify Class where
    qualify (Class nm ts) = Class nm <$> qualify ts

instance Qualify Con where
    qualify (Con nm ts) = Con nm <$> qualify ts

instance Qualify Dec where
    qualify d@(ValD ts body) = 
        onfailq (\msg -> lift $ throw (msg ++ "\n when flattening " ++ pretty d)) $ do
           ts' <- qualify ts
           body' <- qualify body
           return (ValD ts' body')

    -- TODO: qualify type and data constructors.
    qualify (DataD n vars cs) = DataD n vars <$> qualify cs

    -- TODO: qualify class names
    qualify (ClassD nm vars sigs) = do
        sigs' <- mapM qualify sigs
        return (ClassD nm vars sigs')

    qualify (InstD ctx cls meths) = do
        ctx' <- qualify ctx
        cls' <- qualify cls
        meths' <- mapM qualify meths
        return (InstD ctx' cls' meths')

    qualify (PrimD ts) = do
        ts' <- qualify ts
        return (PrimD ts')

instance Qualify Type where
    -- TODO: qualify type
    qualify t@(ConT nm) = do
        -- Is this a type synonym?
        syns <- gets qs_syns
        case HT.lookup nm syns of
            Just t' -> qualify t'
            _ -> return t
    qualify (AppT a b) = do
        a' <- qualify a
        b' <- qualify b
        return (AppT a' b')
    qualify t = return t

instance Qualify Exp where
    -- TODO: qualify data constructors
    qualify e@(LitE {}) = return e
    qualify (ConE (Sig n t)) = do
        t' <- qualify t
        return (ConE (Sig n t'))
    qualify (VarE (Sig n t)) = do
        t' <- qualify t
        bound <- isbound n
        if bound 
            then return (VarE (Sig n t'))
            else do
                n' <- resolve n
                return (VarE (Sig n' t'))
    qualify (AppE f x) = do
        f' <- qualify f
        x' <- qualify x
        return (AppE f' x')
    qualify (LamE (Sig n t) b) = do
        t' <- qualify t
        LamE (Sig n t') <$> (withbound [n] $ qualify b)
    
    qualify (CaseE x (Sig kn kt) y n) = do
        kt' <- qualify kt
        x' <- qualify x
        y' <- qualify y
        n' <- qualify n
        return $ CaseE x' (Sig kn kt') y' n'

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
      r env me@(Module menm imports _ _) = 
        let immediate :: Module -> [Name]
            immediate (Module mnm _ _ ds) = map (const (mnm `nappend` name "." `nappend` n)) (filter (hasName n) ds)
        in do
            imported <- mapM (\(Import mn) -> lookupModule mn env) imports
            let names = map immediate (me : imported)
            case concat names of
                [] -> throw $ "'" ++ pretty n ++ "' not found in module " ++ pretty menm
                [x] -> return x
                xs -> throw $ "'" ++ pretty n ++ "' is ambiguious: " ++ show xs
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
  let syns = mkSyns (concat [s | Module _ _ s _ <- ms])
  (Module _ _ _ d, _) <- runStateT (qualify m) (QS ms (error "not in module") [] syns)
  return d
            

