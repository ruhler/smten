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

{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Smten.Module.Module (
    Module(..), Import(..), Synonym(..), DataDec(..), Deriving(..),
    flatten, flatten1,
    ) where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List(nub)
import Data.Maybe(fromMaybe)

import qualified Smten.HashTable as HT
import Smten.Failable
import Smten.Ppr
import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.Exp
import Smten.Dec


-- | Currently imports are restricted to the form:
-- > import Foo.Bar
-- No hiding or qualification is supported.
data Import = Import Name
    deriving(Show, Eq)

-- type Foo a b ... = ...
data Synonym = Synonym Name [Name] Type 
    deriving (Show, Eq)

-- data Foo a b ... = ...
data DataDec = DataDec Name [TyVar] [ConRec]
    deriving (Show, Eq)

-- deriving instance ctx => cls
data Deriving = Deriving Context Class
    deriving (Show, Eq)

data Module = Module {
    mod_name :: Name,
    mod_imports :: [Import],
    mod_synonyms :: [Synonym],

    -- | A copy of the original data type declarations in the module. This is
    -- recorded here so we have access to the record type constructors for
    -- stand-alone deriving.
    -- Note: mod_decs already contains the desugared declarations
    -- corresponding to this data declaration.
    mod_ddecs :: [DataDec],

    -- | list of stand-alone deriving only.
    mod_derivings :: [Deriving],

    mod_decs :: [Dec]
} deriving(Show, Eq)

instance Ppr Import where
    ppr (Import n) = text "import" <+> ppr n <> semi

instance Ppr Synonym where
    ppr (Synonym n vs t)
      = sep ([text "type", ppr n] 
                ++ map ppr vs
                ++ [text "=", ppr t]) <> semi

instance Ppr Deriving where
    ppr (Deriving ctx cls) = sep [
            text "deriving",
            text "instance",
            ppr ctx,
            ppr cls] <> semi

instance Ppr Module where
    ppr m
        = text "module" <+> ppr (mod_name m) <+> text "where" <+> text "{"
            $+$ nest tabwidth (
                vcat (map ppr (mod_imports m))
                $+$ vcat (map ppr (mod_synonyms m))
                $+$ ppr (mod_decs m)) $+$ text "}"

lookupModule :: Name -> [Module] -> Failable Module
lookupModule n [] = throw $ "module " ++ pretty n ++ " not found"
lookupModule n (m:_) | (n == mod_name m) = return m
lookupModule n (_:ms) = lookupModule n ms

data QS = QS {
    qs_env :: [Module],     -- ^ The environment
    qs_me :: Module,        -- ^ The current module
    qs_bound :: [Name],     -- ^ List of bound variable names
    qs_syns :: HT.HashTable Name ([Name], Type) -- ^ All type synonyms
}

type QualifyM = StateT QS Failable

mkSyns :: [Synonym] -> HT.HashTable Name ([Name], Type)
mkSyns xs = HT.table [(n, (vs, t)) | Synonym n vs t <- xs]

mkDDecs :: [DataDec] -> HT.HashTable Name [ConRec]
mkDDecs xs = HT.table [(n, cs) | DataDec n _ cs <- xs]

onfailq :: (String -> QualifyM a) -> QualifyM a -> QualifyM a
onfailq f q = do
   s <- get
   case (attempt $ runStateT q s) of
     Left msg -> f msg
     Right (v, s') -> put s' >> return v

mename :: QualifyM Name
mename = gets (mod_name . qs_me)

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
    qualify m = do
        modify $ \qs -> qs { qs_me = m }
        sy' <- mapM qualify (mod_synonyms m)
        ds' <- mapM qualify (mod_decs m)
        modify $ \qs -> qs { qs_me = (error "not in module") }
        return $ m { mod_synonyms = sy', mod_decs = ds' }

instance Qualify Synonym where
    qualify (Synonym n vs t) = Synonym n vs <$> qualify t

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

instance Qualify TopExp where
    qualify (TopExp ts body) = do
           ts' <- qualify ts
           body' <- qualify body
           return (TopExp ts' body')

instance Qualify Dec where
    qualify d@(ValD e) = 
        onfailq (\msg -> lift $ throw (msg ++ "\n when flattening " ++ pretty d)) $
            ValD <$> qualify e

    -- TODO: qualify type and data constructors.
    qualify (DataD n vars cs) = DataD n vars <$> qualify cs

    -- TODO: qualify class names
    qualify (ClassD ctx nm vars sigs) = do
        ctx' <- qualify ctx
        sigs' <- mapM qualify sigs
        return (ClassD ctx' nm vars sigs')

    qualify (InstD ctx cls meths) = do
        ctx' <- qualify ctx
        cls' <- qualify cls
        meths' <- mapM qualify meths
        return (InstD ctx' cls' meths')

    qualify (PrimD ts) = do
        ts' <- qualify ts
        return (PrimD ts')

qthrow :: String -> QualifyM a
qthrow = throw

instance Qualify Type where
    qualify t = do
        syns <- gets qs_syns
        case t of
          t | (ConT nm _, args) <- de_appsT t
            , Just (vs, t') <- HT.lookup nm syns ->
                if length vs > length args
                    then qthrow $ "expecting at least "
                             ++ show (length vs)
                             ++ " argument(s) to synonym "
                             ++ pretty nm ++ " in " ++ pretty t
                    else let (bound, rest) = splitAt (length vs) args
                         in qualify (appsT (assign (zip vs bound) t') rest)
          AppT a b -> do
            a' <- qualify a
            b' <- qualify b
            return (AppT a' b')
          t -> return t

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
      hasName n (ValD (TopExp (TopSig nm _ _) _)) = (n == nm)
      hasName n (DataD nm _ _) = (n == nm)
      hasName n (ClassD _ nm _ sigs) =
        let hasns [] = False
            hasns ((TopExp (TopSig snm _ _) _):_) | n == snm = True
            hasns (_:ss) = hasns ss
        in (n == nm) || hasns sigs
      hasName n (InstD {}) = False
      hasName n (PrimD (TopSig nm _ _)) = (n == nm)

      r :: [Module] -> Module -> Failable Name
      r env me = 
        let immediate :: Module -> [Name]
            immediate m = map (const (mod_name m `nappend` name "." `nappend` n)) (filter (hasName n) (mod_decs m))
        in do
            imported <- mapM (\(Import mn) -> lookupModule mn env) (mod_imports me)
            let names = map immediate (me : imported)
            case nub $ concat names of
                [] -> throw $ "'" ++ pretty n ++ "' not found in module " ++ pretty (mod_name me)
                [x] -> return x
                xs -> throw $ "'" ++ pretty n ++ "' is ambiguous: " ++ show xs
  in do
      me <- gets qs_me
      env <- gets qs_env
      lift $ r env me

-- | Flatten a complete module hierarchy.
-- Includes builtin prelude.
flatten :: [Module] -> Failable [Dec]
flatten ms = do
    ds <- mapM (flatten1 ms) ms
    return $ concat (prelude : ds)

-- | Flatten a single module.
flatten1 :: [Module]    -- ^ The environment
            -> Module   -- ^ The module to flatten
            -> Failable [Dec] -- ^ Flattened declarations from the module
flatten1 ms m = do
  let syns = mkSyns $ concatMap mod_synonyms ms
      ddecs = mkDDecs $ concatMap mod_ddecs ms

      mderive :: Deriving -> Failable Dec
      mderive d@(Deriving ctx cls)
        | Class _ [t] <- cls
        , (ct, _) <- de_appsT t
        , Just n <- de_conT ct
        , Just cs <- HT.lookup n ddecs = return (derive ctx cls cs)
        | otherwise = throw $ "unable to perform standalone derive: " ++ pretty d
  derives <- mapM mderive (mod_derivings m)
  mod_decs <$> evalStateT (qualify (m { mod_decs = derives ++ mod_decs m})) (QS ms (error "not in module") [] syns)

