
{-# LANGUAGE PatternGuards #-}

-- Inferred Value Propagation optimization
module Smten.SMT.IVP (ivp) where

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.ExpH
  

-- The result of inferred value propagation, and the set of variables this
-- result depends on.
type IVPResult = (ExpH, Set.Set Name)

type Cache = [(Name, Map.Map EID IVPResult)]

c_empty :: Cache
c_empty = [(name "", Map.empty)]

c_lookup :: EID -> Cache -> Maybe IVPResult
c_lookup x [] = Nothing
c_lookup x ((n, m):ms) = 
 let a = Map.lookup x m
     b = do
        r <- c_lookup x ms
        guard $ not (Set.member n (snd r))
        return r
 in mplus a b

c_insert :: EID -> IVPResult -> Cache -> Cache
c_insert k v ((n, m):ms) = (n, Map.insert k v m) : ms

-- Do IVP.
-- If the result for the expression is cached, use that.
use :: Map.Map Name ExpH -> ExpH -> State Cache IVPResult
use m e
 | Just id <- getid e = do
    mv <- gets $ c_lookup id
    case mv of  
        Just v -> return v
        Nothing -> do
            v <- def m e
            modify $ c_insert id v
            return v
 | otherwise = def m e

-- Run the given computation where Name is modified.
-- So, for the computation, any items in the cache depending on Name are
-- removed. After the computation, cache is updated with any items from the
-- computation not depending on Name.
with :: Name -> State Cache a -> State Cache a
with n q = do
   modify $ (:) (n, Map.empty)
   v <- q
   ((_, m):(n', m'):ms) <- get
   put $ (n', Map.union m' (Map.filter (not . Set.member n . snd) m)):ms
   return v

-- Do IVP.
-- Does not check if the result for the expression is cached.
def :: Map.Map Name ExpH -> ExpH -> State Cache IVPResult
def m e
 | LitEH {} <- e = return (e, Set.empty)
 | ConEH _ n t xs <- e = do
    xs' <- mapM (use m) xs
    return (identify $ \id -> ConEH id n t (map fst xs'), Set.unions (map snd xs'))
 | VarEH (Sig n _) <- e = return (fromMaybe e (Map.lookup n m), Set.singleton n)
 | PrimEH _ _ _ f xs <- e = do
    xs' <- mapM (use m) xs
    return (f (map fst xs'), Set.unions (map snd xs'))
 | AppEH _ f x <- e = do
    (fv, fns) <- use m f
    (xv, xns) <- use m x
    return (appEH fv xv, Set.union fns xns)
 | LamEH _ s t f <- e = error "IVP.def: LamEH"
 | CaseEH _ x k y d <- e = do
    (x', xns) <- use m x
    case x' of
     VarEH (Sig nm t) | t == boolT -> do
        let Just kv = de_boolEH (conEH k)
        (yv, yns) <- with nm $ use (Map.insert nm (boolEH kv) m) y
        (dv, dns) <- with nm $ use (Map.insert nm (boolEH (not kv)) m) d
        return (caseEH x' k yv dv, Set.unions [xns, yns, dns])
     ConEH {} -> do
        (v, vns) <- def m (caseEH x' k y d)
        return (v, Set.union vns xns)
     _ -> do
        (yv, yns) <- use m y
        (dv, dns) <- use m d
        return (caseEH x' k yv dv, Set.unions [xns, yns, dns])
 | ErrorEH {} <- e = return (e, Set.empty)
    

-- Perform inferred value propagation on the given expression.
-- Assumes the expression may be looked at in its entirety.
ivp :: ExpH -> ExpH
ivp e = fst $ evalState (use Map.empty e) c_empty

