
{-# LANGUAGE FlexibleInstances #-}

module Smten.Module.Entity (EntityMap, entities, sources, resolve) where

import Control.Monad.State
import Control.Monad.Writer

import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.List (nub)
import qualified Data.HashMap as Map
import qualified Data.HashSet as Set

import Smten.Name
import Smten.Failable
import Smten.Ppr
import Smten.Dec
import Smten.Module.Module
import Smten.Module.Ppr

-- Entity map maps a local qualified or unqualified name to the list of 
-- fully resolved entity names in scope for that local name. If there is more
-- than one distinct element on the list, it means there is an ambiguity, but
-- that's handled elsewhere.
type EntityMap = Map.Map Name [Name]

data ES = ES {
  -- Map from module name to module
  es_modules :: Map.Map Name Module,

  -- Map from module name to the entities in scope in that module.
  es_entities :: Map.Map Name EntityMap,

  -- Map from module name to the (resolved) entities that module exports.
  es_exports :: Map.Map Name (Set.Set Name),

  -- Map from module name to the (unqualified) locally defined entities in
  -- that module.
  es_locals :: Map.Map Name [Name]
}

type EM m = StateT ES m

instance (MonadErrorSL m) => MonadErrorSL (StateT ES m) where
    errloc = lift errloc


getmod :: (MonadErrorSL m) => Name -> EM m Module
getmod n = do
  mods <- gets es_modules
  case Map.lookup n mods of
    Just m -> return m
    Nothing -> lthrow $ "module " ++ pretty n ++ " not found"

-- Return the list of local (unqualified) names defined by a module.
-- Looks up the result in the cache if it is already there.
-- Saves the result in the cache if it isn't already there.
locals :: (MonadErrorSL m) => Name -> EM m [Name]
locals mn = do
  lcls <- gets es_locals
  case Map.lookup mn lcls of
    Just vs -> return vs
    Nothing -> do
      m <- getmod mn
      let exdec :: Dec -> Writer [Name] ()
          exdec (ValD _ (TopExp (TopSig nm _ _) _)) = tell [unqualified nm]
          exdec (DataD _ nm _ cs) = tell [unqualified nm] >> mapM_ excon cs
          exdec (ClassD _ _ nm _ sigs) = tell [unqualified nm] >> mapM_ exmeth sigs
          exdec (InstD {}) = return ()
          exdec (PrimD _ _ (TopSig nm _ _)) = tell [unqualified nm]

          exmeth :: TopExp -> Writer [Name] ()
          exmeth (TopExp (TopSig snm _ _) _) = tell [unqualified snm]
           
          excon :: Con -> Writer [Name] ()
          excon (Con n _) = tell [unqualified n]
        
          vs = execWriter (mapM exdec (mod_decs m))
      modify $ \s -> s { es_locals = Map.insert mn vs (es_locals s) }
      return vs
      

-- Return the set of (resolved) names exported by a module.
-- Looks up the result in the cache if it is already there.
-- Saves the result in the cache if it isn't already there.
exports :: (Functor m, MonadErrorSL m) => Name -> EM m (Set.Set Name)
exports mn = do
    exs <- gets es_exports
    case Map.lookup mn exs of
       Just vs -> return vs
       Nothing -> do
         m <- getmod mn
         vs <- case (mod_exports m) of
                 Local -> Set.fromList . map (qualified mn) <$> locals mn
                 Exports exs -> do
                   ents <- modents mn
                   let --expnms :: Export -> EM m [Name]
                       expnms (EntityExport n) = do
                            n' <- resolve n ents
                            return [n']
                       expnms (ModuleExport mn) = do
                            let keep n _ = qualification n == mn
                                keeps = Map.filterWithKey keep ents
                            return $ concat (Map.elems keeps)
                   Set.fromList . concat <$> mapM expnms exs
         modify $ \s -> s { es_exports = Map.insert mn vs (es_exports s) }
         return vs

-- Return the set of all entities in scope for the given module.
-- Looks up the result in the cache if it is already there.
-- Saves the result in the cache if it isn't already there.
modents :: (Functor m, MonadErrorSL m) => Name -> EM m EntityMap
modents mn = do
  ents <- gets es_entities
  case Map.lookup mn ents of
     Just vs -> return vs
     Nothing -> do
        m <- getmod mn
        localnms <- locals mn
        let qfd = [(qualified mn n, [qualified mn n]) | n <- localnms]
            unqfd = [(n, [qualified mn n]) | n <- localnms]
            locals = Map.fromList (qfd ++ unqfd)
        imported <- mapM imports (mod_imports m) 
        let vs = Map.unionsWith (++) (locals:imported)
        modify $ \s -> s { es_entities = Map.insert mn vs (es_entities s) }
        return vs

-- Return the entities defined by the single import declaration.
imports :: (Functor m, MonadErrorSL m) => Import -> EM m EntityMap
imports imp@(Import fr as qo spec) = do
  exported <- exports fr
  let imported = case spec of
                    Include ins -> Set.filter (\n -> unqualified n `elem` ins) exported
                    Exclude exs -> Set.filter (\n -> unqualified n `notElem` exs) exported

      qfd = [(qualified as (unqualified n), [n]) | n <- Set.toList imported]
      unqfd = if qo 
                then []
                else [(unqualified n, [n]) | n <- Set.toList imported]
  return $ Map.fromListWith (++) (unqfd ++ qfd)


-- Return the entity map for each of the given modules
entities :: (Functor m, MonadErrorSL m) => [Module] -> m (Map.Map Name EntityMap)
entities ms = do
  let modmap = Map.fromList [(mod_name m, m) | m <- ms]
      getents = do
        mapM_ modents (map mod_name ms)
        gets es_entities
  evalStateT getents (ES modmap Map.empty Map.empty Map.empty)

-- Return the list of source modules needed for the entities defined in the
-- given module.
sources :: (MonadErrorSL m) => Name -> Map.Map Name EntityMap -> m [Name]
sources mn m = do
    case Map.lookup mn m of
        Just es -> do
            let nms = map qualification (concat $ Map.elems es)
                ignore n = nnull n || n == mn
            return (nub $ filter (not . ignore) nms)
        Nothing -> lthrow $ "module " ++ pretty mn ++ " not found"
   
resolve :: (MonadErrorSL m) => Name -> EntityMap -> m Name
resolve n ents = 
  case nub $ fromMaybe [] (Map.lookup n ents) of
     [] -> lthrow $ "'" ++ pretty n ++ "' is not defined"
     [x] -> return x
     xs -> lthrow $ "'" ++ pretty n ++ "' is ambiguous: " ++ show xs

