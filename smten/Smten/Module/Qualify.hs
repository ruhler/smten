
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Module.Qualify (
    qualify
    ) where

import Control.Monad.Writer
import Control.Monad.Reader

import Data.List(nub)
import Data.Functor ((<$>))
import Data.Maybe(fromMaybe, catMaybes)
import qualified Data.HashSet as Set

import Smten.Failable
import qualified Smten.HashTable as HT
import Smten.Location
import Smten.Name
import Smten.Sig
import Smten.Ppr
import Smten.Type
import Smten.Exp
import Smten.Dec
import Smten.Module.Module

data QS = QS {
    qs_env :: [Module],     -- ^ The environment
    qs_me :: Module,        -- ^ The current module
    qs_bound :: [Name],     -- ^ List of bound variable names
    qs_syns :: HT.HashTable Name ([Name], Type), -- ^ All type synonyms
    qs_exports :: HT.HashTable Name (Set.Set Name),      -- ^ Module exports
    qs_loc :: Location      -- ^ The current location
}

type QualifyM = ReaderT QS Failable

instance MonadErrorSL QualifyM where
    errloc = asks qs_loc

withloc :: Location -> QualifyM a -> QualifyM a
withloc l = local (\r -> r { qs_loc = l } )

mkSyns :: [Synonym] -> HT.HashTable Name ([Name], Type)
mkSyns xs = HT.table [(n, (vs, t)) | Synonym n vs t <- xs]

mkExports :: [Module] -> HT.HashTable Name (Set.Set Name)
mkExports xs = HT.table [(mod_name m, exports m) | m <- xs]

mename :: QualifyM Name
mename = asks (mod_name . qs_me)

withbound :: [Name] -> QualifyM a -> QualifyM a
withbound binds = local (\qs -> qs { qs_bound = binds ++ qs_bound qs })

isbound :: Name -> QualifyM Bool
isbound n = do
    bound <- asks qs_bound
    return $ n `elem` bound

class Qualify a where
    -- | Resolve all unqualified names in the given object.
    qualifyM :: a -> QualifyM a 

instance Qualify Module where
    qualifyM m = local (\qs -> qs { qs_me = m }) $ do
        sy' <- mapM qualifyM (mod_synonyms m)
        ds' <- mapM qualifyM (mod_decs m)
        return $ m { mod_synonyms = sy', mod_decs = ds' }

instance Qualify Synonym where
    qualifyM (Synonym n vs t) = Synonym n vs <$> qualifyM t

qdefine :: Name -> QualifyM Name
qdefine n 
 | isqualified n = return n
 | otherwise = do
    menm <- mename
    return $ qualified menm n

instance Qualify TopSig where
    qualifyM (TopSig nm ctx t) = do
        nm' <- qdefine nm
        ctx' <- qualifyM ctx
        t' <- qualifyM t
        return (TopSig nm' ctx' t')

instance (Qualify a) => Qualify [a] where
    qualifyM = mapM qualifyM

instance Qualify Class where
    qualifyM (Class nm ts) = do
        nm' <- qualifyM nm
        ts' <- qualifyM ts
        return $ Class nm' ts'

instance Qualify Con where
    qualifyM (Con nm ts) = do
        nm' <- qdefine nm
        ts' <- qualifyM ts
        return $ Con nm' ts'

instance Qualify TopExp where
    qualifyM (TopExp ts body) = do
           ts' <- qualifyM ts
           body' <- qualifyM body
           return (TopExp ts' body')

instance Qualify Dec where
    qualifyM d@(ValD l e) = withloc l $ ValD l <$> qualifyM e

    qualifyM (DataD l n vars cs) = withloc l $ do
        n' <- qdefine n
        cs' <- qualifyM cs
        return $ DataD l n' vars cs'

    qualifyM (ClassD l ctx nm vars sigs) = withloc l $ do
        nm' <- qdefine nm
        ctx' <- qualifyM ctx
        sigs' <- mapM qualifyM sigs
        return (ClassD l ctx' nm' vars sigs')

    qualifyM (InstD l ctx cls meths) = withloc l $ do
        ctx' <- qualifyM ctx
        cls' <- qualifyM cls
        meths' <- mapM qualifyM meths
        return (InstD l ctx' cls' meths')

    qualifyM (PrimD l ts) = withloc l $ do
        ts' <- qualifyM ts
        return (PrimD l ts')

instance Qualify Type where
    qualifyM t = do
        syns <- asks qs_syns
        case t of
          t | (ConT nm _, args) <- de_appsT t
            , Just (vs, t') <- HT.lookup nm syns ->
                if length vs > length args
                    then lthrow $ "expecting at least "
                             ++ show (length vs)
                             ++ " argument(s) to synonym "
                             ++ pretty nm ++ " in " ++ pretty t
                    else let (bound, rest) = splitAt (length vs) args
                         in qualifyM (appsT (assign (zip vs bound) t') rest)
          ConT n k -> do
            n' <- qualifyM n
            return (ConT n' k)
          AppT a b -> do
            a' <- qualifyM a
            b' <- qualifyM b
            return (AppT a' b')
          VarT {} -> return t
          NumT {} -> return t
          OpT {} -> return t
          UnknownT {} -> return t

instance Qualify Exp where
    qualifyM e@(LitE {}) = return e
    qualifyM (ConE l (Sig n t)) = withloc l $ do
        n' <- qualifyM n
        t' <- qualifyM t
        return (ConE l (Sig n' t'))
    qualifyM (VarE l (Sig n t)) = withloc l $ do
        t' <- qualifyM t
        bound <- isbound n
        if bound 
            then return (VarE l (Sig n t'))
            else do
                n' <- resolve n
                return (VarE l (Sig n' t'))
    qualifyM (AppE l f x) = withloc l $ do
        f' <- qualifyM f
        x' <- qualifyM x
        return (AppE l f' x')
    qualifyM (LamE l (Sig n t) b) = withloc l $ do
        t' <- qualifyM t
        LamE l (Sig n t') <$> (withbound [n] $ qualifyM b)
    
    qualifyM (CaseE l x (Sig kn kt) y n) = withloc l $ do
        kn' <- qualifyM kn
        kt' <- qualifyM kt
        x' <- qualifyM x
        y' <- qualifyM y
        n' <- qualifyM n
        return $ CaseE l x' (Sig kn' kt') y' n'

instance Qualify Method where
    qualifyM (Method nm e) = do
        e' <- qualifyM e
        nm' <- qualifyM nm
        return (Method nm' e')

instance Qualify Name where
    qualifyM = resolve

-- Return the set of entities exported by the given module.
exports :: Module -> (Set.Set Name)
exports m =
  let exdec :: Dec -> Writer [Name] ()
      exdec (ValD _ (TopExp (TopSig nm _ _) _)) = tell [unqualified nm]
      exdec (DataD _ nm _ cs) = tell [unqualified nm] >> mapM_ excon cs
      exdec (ClassD _ _ nm _ sigs) = tell [unqualified nm] >> mapM_ exmeth sigs
      exdec (InstD {}) = return ()
      exdec (PrimD _ (TopSig nm _ _)) = tell [unqualified nm]

      exmeth :: TopExp -> Writer [Name] ()
      exmeth (TopExp (TopSig snm _ _) _) = tell [unqualified snm]
       
      excon :: Con -> Writer [Name] ()
      excon (Con n _) = tell [unqualified n]
    
  in Set.fromList $ execWriter (mapM exdec (mod_decs m))

-- Resolve the given name based on the given import.
-- Returns the unique name for the entity if it is accessible via this import.
resolvein :: Name -> Import -> QualifyM (Maybe Name)
resolvein n (Import fr as qo) = do
  exps <- asks qs_exports
  case HT.lookup fr exps of
    Just xs -> do
      let uqn = unqualified n
          qn = qualification n
      return $ do
          guard $ uqn `Set.member` xs
          guard $ qn == as || (not qo && nnull qn)
          return $ qualified fr uqn
    Nothing -> lthrow $ "Module " ++ pretty fr ++ " not found"
        
-- | Return the unique name for the entity referred to by the given name.
resolve :: Name -> QualifyM Name
resolve n = do
  me <- asks qs_me
  let meimport = Import (mod_name me) (mod_name me) False
  finds <- mapM (resolvein n) (meimport : mod_imports me)
  case nub $ catMaybes finds of
      [] -> lthrow $ "'" ++ pretty n ++ "' not found in module " ++ pretty (mod_name me)
      [x] -> return x
      xs -> lthrow $ "'" ++ pretty n ++ "' is ambiguous: " ++ show xs

-- | Perform name resolution on the given modules.
qualify :: [Module] -> Failable [Module]
qualify ms = do
  let syns = mkSyns $ concatMap mod_synonyms ms
      exps = mkExports ms
  runReaderT (qualifyM ms) (QS ms (error "not in module") [] syns exps lunknown)

