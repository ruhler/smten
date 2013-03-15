
{-# LANGUAGE PatternGuards #-}

-- Inferred Value Propagation optimization
module Smten.SMT.IVP (ivp) where

import Control.Monad.State

import Data.Functor((<$>))
import qualified Data.Map as Map
import qualified Data.HashMap as HashMap
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Strict
  

-- The result of inferred value propagation, and the set of variables this
-- result depends on.
type IVPResult = (ExpH, Set.Set Name)

type Context = Map.Map Name Bool
type ContextMap = [(Context, IVPResult)]
type Cache = HashMap.Map EID ContextMap

-- Lookup in a value in the context map.
cm_lookup :: Context -> ContextMap -> Maybe IVPResult
cm_lookup _ [] = Nothing
cm_lookup m ((c, r@(e, s)):cs) = {-# SCC "CM_LOOKUP" #-}
 let ishere = c == cm_restrict m s
 in if ishere
        then Just r
        else cm_lookup m cs

cm_insert :: Context -> IVPResult -> ContextMap -> ContextMap
cm_insert ctx v@(_, s) cm = {-# SCC "CM_INSERT" #-} (cm_restrict ctx s, v) : cm

cm_empty :: ContextMap 
cm_empty = []

cm_single :: Context -> IVPResult -> ContextMap
cm_single ctx v = cm_insert ctx v cm_empty

cm_restrict :: Context -> Set.Set Name -> Context
cm_restrict c s = Map.filterWithKey (\k _ -> k `Set.member` s) c

-- Do IVP.
-- If the result for the expression is cached, use that.
use :: Context -> ExpH -> State Cache IVPResult
use m e
 | Just id <- getid e = do
    cache <- get
    case {-# SCC "CACHE_LOOKUP" #-} HashMap.lookup id cache of  
        Just cm ->
           case cm_lookup m cm of
              Just v -> return v
              Nothing -> do
                v <- def m e
                modifyS $ {-# SCC "CACHE_INSERT" #-} HashMap.insert id (cm_insert m v cm) 
                return v
        Nothing -> do
            v <- def m e
            modifyS $ {-# SCC "CACHE_INSERT" #-} HashMap.insert id (cm_single m v)
            return v
 | otherwise = def m e

-- Do IVP.
-- Does not check if the result for the expression is cached.
def :: Context -> ExpH -> State Cache IVPResult
def m e
 | LitEH {} <- e = return (e, Set.empty)
 | ConEH _ n t xs <- e = do
    xs' <- mapM (use m) xs
    return (identify $ \id -> ConEH id n t (map fst xs'), Set.unions (map snd xs'))
 | VarEH (Sig n _) <- e = return (fromMaybe e (boolEH <$> Map.lookup n m), Set.singleton n)
 | PrimEH _ _ _ f xs <- e = do
    xs' <- mapM (use m) xs
    return (f (map fst xs'), Set.unions (map snd xs'))
 | LamEH _ s t f <- e = error "IVP.def: LamEH"
 | IfEH _ x y d <- e = do
    (x', xns) <- use m x
    case x' of
     VarEH (Sig nm t) | t == boolT -> do
        (yv, yns) <- use (Map.insert nm True m) y
        (dv, dns) <- use (Map.insert nm False m) d
        return (ifEH x' yv dv, Set.unions [xns, yns, dns])
     ConEH {} -> do
        (v, vns) <- def m (ifEH x' y d)
        return (v, Set.union vns xns)
     _ -> do
        (yv, yns) <- use m y
        (dv, dns) <- use m d
        return (ifEH x' yv dv, Set.unions [xns, yns, dns])
 | ErrorEH {} <- e = return (e, Set.empty)
    

-- Perform inferred value propagation on the given expression.
-- Assumes the expression may be looked at in its entirety.
ivp :: ExpH -> ExpH
ivp e = fst $ evalState (use Map.empty e) HashMap.empty

