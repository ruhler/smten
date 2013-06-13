
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Typing.KInfer (
    kindinfer,
    ) where

import Control.Monad.Error
import Control.Monad.State
import Data.Functor((<$>))
import Data.Maybe(catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Smten.Name
import Smten.Location
import Smten.Type
import Smten.Ppr
import Smten.Failable
import Smten.Dec
import Smten.Module

import Smten.Typing.ConTs
import Smten.Typing.KSolver

data SS = SS {
    ss_env :: Env,
    ss_done :: Set.Set Name,
    ss_groups :: [Set.Set Name]
}

initSS :: Env -> SS
initSS e = SS e Set.empty []

type SortM = StateT SS

instance (MonadErrorSL m) => MonadErrorSL (SortM m) where
    errloc = lift errloc

maybeLookupTypeD :: Env -> Name -> Maybe Dec
maybeLookupTypeD e n = attemptM $ lookupTypeD e n

-- Determine the order in which to do kind inference on the type constructors
sort :: (MonadPlus m, MonadErrorSL m) => Env -> [Dec] -> m [[Name]]
sort e ds = do
  let tycons = conTs ds
      
  s <- execStateT (mapM (sortin Set.empty) (Set.elems tycons)) (initSS e)
  return $ map Set.elems (reverse (ss_groups s))

-- Sort the given type constructor into ss_groups, assuming the given set of
-- type constructors depends on it.
--
-- Returns the set of type constructors which mutually recursively depend on
-- an element in the given set of type constructors.
sortin :: (MonadPlus m, MonadErrorSL m) => Set.Set Name -> Name -> SortM m (Set.Set Name)
sortin pending ty
  | ty `Set.member` pending = return (Set.singleton ty)
  | otherwise = do
      done <- gets $ ss_done
      if ty `Set.member` done
          then return Set.empty
          else do
              env <- gets ss_env
              let deps = case maybeLookupTypeD env ty of
                             Nothing -> Set.empty
                             Just dec -> conTs dec
              sins <- mapM (sortin (Set.insert ty pending)) (Set.elems deps)
              let v = Set.insert ty (Set.unions sins)
              if Set.null (Set.intersection pending v)
                then do
                    modify $ \ss -> ss { ss_groups = v : ss_groups ss,
                                         ss_done = Set.union (ss_done ss) v }
                    return Set.empty
                else return v
              
-- Sort the given declarations into dependency groups.
sortd :: (MonadPlus m, MonadErrorSL m) =>  [Dec] -> m [[Dec]]
sortd ds = do
  let e = mkEnv ds
  tys <- sort e ds
  let f :: [Name] -> [Dec]
      f = catMaybes . map (maybeLookupTypeD e)

      tyds = map f tys

      isnonty :: Dec -> Bool
      isnonty d
        | ValD {} <- d = True
        | InstD {} <- d = True
        | PrimD {} <- d = True
        | AsInHaskellD {} <- d = True
        | otherwise = False

      ntyds = filter isnonty ds
  return $ tyds ++ [[ntyd] | ntyd <- ntyds]

-- | Replace all UnknownK with new variable kinds.
class Deunknown a where
    deunknown :: a -> KIM a

instance Deunknown Kind where
    deunknown k
      | UnknownK <- k = newvk
      | ArrowK a b <- k = do
            a' <- deunknown a
            b' <- deunknown b
            return $ ArrowK a' b'
      | otherwise = return k

instance Deunknown TopExp where
    deunknown (TopExp t e) = do
        t' <- deunknown t
        return (TopExp t' e)

instance Deunknown Dec where
    deunknown d
      | ValD l e <- d = ValD l <$> deunknown e
      | DataD l n vs cs <- d = do
            vs' <- deunknown vs
            cs' <- deunknown cs
            return $ DataD l n vs' cs'
      | ClassD l ctx n vs ts <- d = do
            ctx' <- deunknown ctx
            vs' <- deunknown vs
            ts' <- deunknown ts
            return $ ClassD l ctx' n vs' ts'
      | InstD l ctx cls ms <- d = do
            ctx' <- deunknown ctx
            cls' <- deunknown cls
            return $ InstD l ctx' cls' ms
      | PrimD l n t <- d = PrimD l n <$> deunknown t
      | AsInHaskellD l a b <- d = return d

instance (Deunknown a) => Deunknown [a] where
    deunknown = mapM deunknown

instance Deunknown TyVar where
    deunknown (TyVar n k) = TyVar n <$> deunknown k

instance Deunknown Con where
    deunknown (Con n ts) = Con n <$> deunknown ts

instance Deunknown Class where
    deunknown (Class n ts) = Class n <$> deunknown ts

instance Deunknown TopSig where
    deunknown (TopSig n ctx ty) = do
        ctx' <- deunknown ctx
        ty' <- deunknown ty
        return $ TopSig n ctx' ty'

instance Deunknown Type where
    deunknown t
      | ConT n k <- t = ConT n <$> deunknown k
      | AppT a b <- t = do
            a' <- deunknown a
            b' <- deunknown b
            return $ AppT a' b'
      | VarT n k <- t = VarT n <$> deunknown k
      | otherwise = return t

  
  
data KS = KS {
    ks_cons :: [(Kind, Kind)], -- ^ generated kind constraints
    ks_nvk :: Integer,         -- ^ next variable kind ID to use.
    ks_tcs :: Map.Map Name Kind, -- ^ type and var constructor kinds.
    ks_loc :: Location
}

type KIM = StateT KS Failable

withloc :: Location -> KIM a -> KIM a
withloc l q = do
  oldl <- gets ks_loc
  modify $ \ks -> ks { ks_loc = l }
  v <- q
  modify $ \ks -> ks { ks_loc = oldl }
  return v

instance (MonadErrorSL KIM) where
    errloc = gets ks_loc

-- Create a new variable kind.
newvk :: KIM Kind
newvk = do
    ks <- get
    put $ ks { ks_nvk = ks_nvk ks + 1 }
    return . VarK $ ks_nvk ks

addc :: Kind -> Kind -> KIM ()
addc a b = modify $ \ks -> ks { ks_cons = (a,b) : ks_cons ks }

-- Look up the kind of a type constructor
tckind :: Name -> KIM Kind
tckind n = do
    m <- gets ks_tcs
    case Map.lookup n m of
        Just k -> return k
        Nothing -> lthrow $ "type constructor " ++ pretty n ++ " not in scope"

class Constrain a where
    constrain :: a -> KIM ()

instance (Constrain a) => Constrain [a] where
    constrain = mapM_ constrain

instance Constrain TopSig where
    constrain (TopSig _ ctx ty) = do
        tcs <- gets ks_tcs
        let f :: (Name, Kind) -> KIM [(Name, Kind)]
            f (n, k) = case Map.lookup n tcs of
                            Just v -> addc v k >> return []
                            Nothing -> return [(n, k)]
        vsl <- concat <$> mapM f (varTs ty)
        withtcs (Map.fromList vsl) $ do
            k <- constrainT ty
            addc StarK k
            constrain ctx

instance Constrain Con where
    constrain (Con _ ts) = do
        ks <- mapM constrainT ts
        mapM_ (addc StarK) ks

instance Constrain Class where
    constrain (Class n ts) = constrain ts

instance Constrain Type where
    constrain t = constrainT t >> return ()

instance Constrain TopExp where
    constrain (TopExp t _) = constrain t

instance Constrain Dec where
    constrain d
      | ValD l e <- d = withloc l $ constrain e
      | DataD l _ vs cs <- d = withloc l $ do
          withtcs (Map.fromList [(n, k) | TyVar n k <- vs]) (constrain cs)
      | ClassD l ctx _ vs ts <- d = withloc l $ do
          let tcs = Map.fromList [(n, k) | TyVar n k <- vs]
          withtcs tcs $ do
            constrain ctx
            constrain ts
      | InstD l ctx cls _ <- d = withloc l $ do
          let vs = varTs cls
          withtcs (Map.fromList vs) $ do
              constrain ctx
              constrain cls
      | PrimD l _ t <- d = withloc l $ constrain t
      | AsInHaskellD {} <- d = return ()
        
           

withtcs :: Map.Map Name Kind -> KIM a -> KIM a
withtcs m x = do
    tcs <- gets ks_tcs
    modify $ \ks -> ks { ks_tcs = Map.union m tcs }
    r <- x
    modify $ \ks -> ks { ks_tcs = tcs }
    return r
    
    

constrainT :: Type -> KIM Kind
constrainT t
 | ConT n k <- t = do
      kw <- tckind n
      addc k kw
      return k
 | AppT a b <- t = do
      ka <- constrainT a
      kb <- constrainT b
      k1 <- newvk
      k2 <- newvk
      addc ka (ArrowK k1 k2)
      addc kb k1
      return k2
  | VarT n k <- t = do
      kw <- tckind n
      addc k kw
      return k
  | NumT {} <- t = return NumK
  | OpT _ a b <- t = do
       ka <- constrainT a
       addc NumK ka
       kb <- constrainT b
       addc NumK kb
       return NumK
  | UnknownT <- t = return UnknownK

-- Extract the kind definitions from the given declarations.
gettcs :: [Dec] -> Map.Map Name Kind
gettcs [] = Map.empty
gettcs (x:xs) =
   let m = gettcs xs
   in case x of
        DataD _ n vs _ ->
          let k = arrowKs $ [k | TyVar _ k <- vs] ++ [StarK]
          in Map.insert n k m
        ClassD _ _ n vs _ -> 
          let k = arrowKs $ [k | TyVar _ k <- vs] ++ [StarK]
          in Map.insert n k m
        _ -> m

arrowKs :: [Kind] -> Kind
arrowKs [k] = k
arrowKs (k:ks) = ArrowK k (arrowKs ks)

group :: [Dec] -> KIM [Dec]
group ds = do
    ds' <- deunknown ds
    let temptcs = gettcs ds'
    withtcs temptcs (constrain ds')
    cons <- gets ks_cons
    let solution = ksolve cons
        ds_inferred = assignkl (\n -> Map.lookup n solution) ds'
        tcs = gettcs ds_inferred
    modify $ \ks -> ks { ks_cons = [], ks_tcs = Map.union tcs (ks_tcs ks) }
    return ds_inferred

groups :: [[Dec]] -> KIM [Dec]
groups ds = concat <$> mapM group ds

kimod :: Env -> Module -> KIM Module
kimod e m = do
    sorted <- sortd (mod_decs m)
    grouped <- groups sorted
    return $ m { mod_decs = grouped }

-- | Do kind inference on the given modules.
-- The modules are assumed to be sorted in dependency order, where later
-- elements in the list depend on earlier elements.
--
-- Note: this requires modules be non-recursive.
kindinfer :: [Module] -> Failable [Module]
kindinfer ms = {-# SCC "KindInfer" #-}
    evalStateT (mapM (kimod (environ ms)) ms) (KS [] 0 Map.empty lunknown)

