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
    sderive, flatten, flatten1,
    ) where

import Control.Monad.State
import Data.Functor ((<$>))
import Data.List(nub)
import Data.Maybe(fromMaybe, catMaybes)

import qualified Smten.HashTable as HT
import Smten.Failable
import Smten.Ppr
import Smten.Location
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
data Deriving = Deriving Location Context Class
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
    ppr (Deriving _ ctx cls) = sep [
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
    qualify d@(ValD l e) = onfailq (lthrow l) $ ValD l <$> qualify e

    -- TODO: qualify type and data constructors.
    qualify (DataD l n vars cs) = DataD l n vars <$> qualify cs

    -- TODO: qualify class names
    qualify (ClassD l ctx nm vars sigs) = do
        ctx' <- qualify ctx
        sigs' <- mapM qualify sigs
        return (ClassD l ctx' nm vars sigs')

    qualify (InstD l ctx cls meths) = do
        ctx' <- qualify ctx
        cls' <- qualify cls
        meths' <- mapM qualify meths
        return (InstD l ctx' cls' meths')

    qualify (PrimD l ts) = do
        ts' <- qualify ts
        return (PrimD l ts')

instance Qualify Type where
    qualify t = do
        syns <- gets qs_syns
        case t of
          t | (ConT nm _, args) <- de_appsT t
            , Just (vs, t') <- HT.lookup nm syns ->
                if length vs > length args
                    then throw $ "expecting at least "
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
    qualify (ConE l (Sig n t)) = do
        t' <- qualify t
        return (ConE l (Sig n t'))
    qualify (VarE l (Sig n t)) = do
        t' <- qualify t
        bound <- isbound n
        if bound 
            then return (VarE l (Sig n t'))
            else do
                n' <- resolve n
                return (VarE l (Sig n' t'))
    qualify (AppE l f x) = do
        f' <- qualify f
        x' <- qualify x
        return (AppE l f' x')
    qualify (LamE l (Sig n t) b) = do
        t' <- qualify t
        LamE l (Sig n t') <$> (withbound [n] $ qualify b)
    
    qualify (CaseE l x (Sig kn kt) y n) = do
        kt' <- qualify kt
        x' <- qualify x
        y' <- qualify y
        n' <- qualify n
        return $ CaseE l x' (Sig kn kt') y' n'

instance Qualify Method where
    qualify (Method nm e) = do
        e' <- qualify e
        nm' <- resolve nm
        return (Method nm' e')

-- Resolve the given name based on the given import.
-- Returns the unique name for the entity if it is accessible via this import.
resolvein :: Name -> Import -> QualifyM (Maybe Name)
resolvein n (Import mn) = do
  mods <- gets qs_env
  let [mod] = filter (\m -> mod_name m == mn) mods

      hasName :: Dec -> Bool
      hasName (ValD _ (TopExp (TopSig nm _ _) _)) = (n == nm)
      hasName (DataD _ nm _ _) = (n == nm)
      hasName (ClassD _ _ nm _ sigs) =
        let hasns [] = False
            hasns ((TopExp (TopSig snm _ _) _):_) | n == snm = True
            hasns (_:ss) = hasns ss
        in (n == nm) || hasns sigs
      hasName (InstD {}) = False
      hasName (PrimD _ (TopSig nm _ _)) = (n == nm)

      matches = filter hasName (mod_decs mod)
      fqn = mod_name mod `nappend` name "." `nappend` n
  case matches of
     [] -> return Nothing
     [x] -> return (Just fqn)
     _ -> throw $ "'" ++ pretty fqn ++ "' is defined multiple times in"
        
-- | Return the unique name for the entity referred to by the given name.
resolve :: Name -> QualifyM Name
resolve n = do
  me <- gets qs_me
  let meimport = Import (mod_name me)
  finds <- mapM (resolvein n) (meimport : mod_imports me)
  case nub $ catMaybes finds of
      [] -> throw $ "'" ++ pretty n ++ "' not found in module " ++ pretty (mod_name me)
      [x] -> return x
      xs -> throw $ "'" ++ pretty n ++ "' is ambiguous: " ++ show xs

-- | Flatten a complete module hierarchy.
-- Includes builtin prelude.
flatten :: [Module] -> Failable [Dec]
flatten ms = do
    ds <- mapM (flatten1 ms) ms
    return $ concat (prelude : ds)

-- | Flatten a single module.
-- Assumes standalone deriving has already been performed.
flatten1 :: [Module]    -- ^ The environment
            -> Module   -- ^ The module to flatten
            -> Failable [Dec] -- ^ Flattened declarations from the module
flatten1 ms m = do
  let syns = mkSyns $ concatMap mod_synonyms ms
  mod_decs <$> evalStateT (qualify m) (QS ms (error "not in module") [] syns)

-- Perform standalone derivings in the given module.
sderive :: [Module] -> Module -> Failable Module
sderive ms m = do
  let ddecs = mkDDecs $ concatMap mod_ddecs ms

      mderive :: Deriving -> Failable Dec
      mderive d@(Deriving loc ctx cls)
        | Class _ [t] <- cls
        , (ct, _) <- de_appsT t
        , Just n <- de_conT ct
        , Just cs <- HT.lookup n ddecs = return (derive loc ctx cls cs)
        | otherwise = lthrow loc $ "unable to perform standalone derive"
  derives <- mapM mderive (mod_derivings m)
  return $ m { mod_decs = derives ++ mod_decs m }
  
