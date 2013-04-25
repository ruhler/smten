
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Module.Qualify (
    qualify
    ) where

import Control.Monad.Reader

import Data.Functor ((<$>))
import qualified Data.HashMap as Map

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
import Smten.Module.Entity

data QS = QS {
    qs_env :: [Module],     -- ^ The environment
    qs_me :: Module,        -- ^ The current module
    qs_bound :: [Name],     -- ^ List of bound variable names
    qs_syns :: HT.HashTable Name ([Name], Type), -- ^ All type synonyms
    qs_entities :: Map.Map Name EntityMap,       -- ^ Module entities
    qs_loc :: Location      -- ^ The current location
}

type QualifyM = ReaderT QS Failable

instance MonadErrorSL QualifyM where
    errloc = asks qs_loc

withloc :: Location -> QualifyM a -> QualifyM a
withloc l = local (\r -> r { qs_loc = l } )

mkSyns :: [Synonym] -> HT.HashTable Name ([Name], Type)
mkSyns xs = HT.table [(n, (vs, t)) | Synonym n vs t <- xs]

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
        ents <- asks qs_entities
        impmns <- sources (mod_name m) ents
        let imps = [Import n n True (Exclude []) | n <- impmns]
        return $ m { mod_synonyms = sy', mod_decs = ds', mod_imports = imps }

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
    qualifyM (ConE l s) = withloc l $ do
        s' <- qualifyM s
        return (ConE l s')
    qualifyM (VarE l (Sig n t)) = withloc l $ do
        t' <- qualifyM t
        bound <- isbound n
        if bound 
            then return (VarE l (Sig n t'))
            else do
                n' <- qualifyM n
                return (VarE l (Sig n' t'))
    qualifyM (AppE l f x) = withloc l $ do
        f' <- qualifyM f
        x' <- qualifyM x
        return (AppE l f' x')
    qualifyM (LamE l (Sig n t) b) = withloc l $ do
        t' <- qualifyM t
        LamE l (Sig n t') <$> (withbound [n] $ qualifyM b)
    
    qualifyM (CaseE l x k y n) = withloc l $ do
        k' <- qualifyM k
        x' <- qualifyM x
        y' <- qualifyM y
        n' <- qualifyM n
        return $ CaseE l x' k' y' n'

    qualifyM (LetE l bs x) = withloc l $ withbound [n | (Sig n _, _) <- bs] $ do
        bs' <- qualifyM bs
        x' <- qualifyM x
        return $ LetE l bs' x'

instance Qualify (Sig, Exp) where
    qualifyM (Sig n t, e) = do
        t' <- qualifyM t
        e' <- qualifyM e
        return (Sig n t', e')

instance Qualify Sig where
    qualifyM (Sig n t) = do
        n' <- qualifyM n
        t' <- qualifyM t
        return (Sig n' t')

instance Qualify Method where
    qualifyM (Method nm e) = do
        e' <- qualifyM e
        nm' <- qualifyM nm
        return (Method nm' e')

instance Qualify Name where
    qualifyM n = do
        me <- asks qs_me
        allents <- asks qs_entities
        myents <- case Map.lookup (mod_name me) allents of
                      Just ents -> return ents
                      Nothing -> lthrow $ "module " ++ pretty (mod_name me) ++ " not found"
        resolve n myents

-- | Perform name resolution on the given modules.
qualify :: [Module] -> Failable [Module]
qualify ms = {-# SCC "Qualify" #-} do
  let syns = mkSyns $ concatMap mod_synonyms ms
  ents <- entities ms
  runReaderT (qualifyM ms) (QS ms (error "not in module") [] syns ents lunknown)

