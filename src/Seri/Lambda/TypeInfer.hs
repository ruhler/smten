
{-# LANGUAGE FlexibleInstances #-}

module Seri.Lambda.TypeInfer (
        typeinfer
    ) where

import Debug.Trace

import Control.Monad.State
import Data.Generics

import Seri.Failable
import Seri.Lambda.Env
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types
import Seri.Lambda.TypeSolver

-- | Perform type inference on the given declarations.
-- Types UnknownT are inferred.
-- Variable info UnknownVI is inferred.
--
-- The returned expression may have incorrectly inferred types if the
-- expression doesn't type check, so you should run typecheck after inference
-- to make sure it's valid.
typeinfer :: [Dec] -> Failable [Dec]
typeinfer ds = do
    ds' <- mapM (inferdec ds) ds
    --trace ("prevarize: " ++ pretty ds') (return ())
    return $ varize ds'
    
-- Run inference on a single declaration, given the environment.
inferdec :: [Dec] -> Dec -> Failable Dec
inferdec ds (ValD (Sig n t) e) = do
    e' <- inferexp ds t e
    return $ ValD (Sig n t) e'
inferdec ds d@(DataD {}) = return d
inferdec ds d@(ClassD {}) = return d
inferdec ds (InstD (Class cn ts) ms) =
  let infermethod :: Method -> Failable Method
      infermethod (Method n e) = do
         t <- lookupVarType (mkenv ds n)
         ClassD _ vars _ <- lookupClassD (mkenv ds ()) cn
         e' <- inferexp ds (assign (zip vars ts) t) e
         return (Method n e')
  in do
    ms' <- mapM infermethod ms
    return (InstD (Class cn ts) ms')

inferexp :: [Dec] -> Type -> Exp -> Failable Exp
inferexp ds t e = do
 let (e', id) = runState (deunknown e) 1
 let ticomp = do
         te' <- constrain e'
         addc (unforallT t) te'
 (_, TIS _ cons _ _) <- runStateT ticomp (TIS id [] [] ds)
 sol <- solve cons
 --trace ("e': " ++ pretty e') (return ())
 --trace ("constraints: " ++ pretty cons) (return ())
 --trace ("solution: " ++ pretty sol) (return ())
 return $ replace sol e'


-- | Replace all UnknownT with new variable types.
-- State is the id of the next free type variable to use.
deunknown :: (Data e) => e -> State Integer e
deunknown =
    let ununt UnknownT = do
            id <- get
            put (id+1)
            return (VarT $ "~" ++ show id)
        ununt t = return t
    in everywhereM (mkM ununt) 


data TIS = TIS {
    ti_varid :: Integer,        -- ^ The next free VarT id
    ti_cons :: [(Type, Type)],  -- ^ A list of accumulated type constraints
    ti_tenv :: [(Name, Type)],  -- ^ Types of bound variables in scope
    ti_decs :: [Dec]            -- ^ The environment
}

type TI = StateT TIS Failable

-- | Add a type constraint
addc :: Type -> Type -> TI ()
addc a b = modify $ \ti -> ti { ti_cons = (a, b) : (ti_cons ti) }

-- | Return a new variable type.
-- State is the id of the next free type variable to use.
newvt :: TI Type
newvt = do
    n <- newvtn
    return $ VarT n

newvtn :: TI Name
newvtn = do
    id <- gets ti_varid
    modify $ \ti -> ti { ti_varid = (id + 1) }
    return ("~" ++ show id)

-- | Run type checking with additional bound variable's in scope.
scoped :: [(Name, Type)] -> TI a -> TI a
scoped vars x = do
    tenv <- gets ti_tenv
    modify $ \ti -> ti { ti_tenv = vars ++ tenv }
    r <- x
    modify $ \ti -> ti { ti_tenv = tenv }
    return r

enved :: a -> TI (Env a)
enved x = do
    decs <- gets ti_decs
    return $ mkenv decs x

class Constrain a where
    -- | Generate type constraints for an expression, assuming no UnknownT types
    -- are in it.
    --
    -- Returns the type of the thing being constrained.
    constrain :: a -> TI Type

instance Constrain Exp where
    constrain (IntegerE {}) = return integerT
    constrain (PrimE (Sig _ t)) = return t
    constrain (CaseE e ms) = do
        te <- constrain e
        tps <- mapM constrain [p | Match p _ <- ms]
        tms <- mapM constrain ms
        sequence_ [addc te tp | tp <- tps]
        sequence_ [addc (head tms) tm | tm <- tail tms]
        return (head tms)
    constrain (AppE f x) = do
        tf <- constrain f
        tx <- constrain x
        it <- newvt
        ot <- newvt
        addc (arrowsT [it, ot]) tf
        addc it tx
        return ot
    constrain (LamE (Sig n t) b) = do
        bt <- scoped [(n, t)] (constrain b)
        return (arrowsT [t, bt])
    constrain (ConE (Sig n t)) = do
        en <- enved n
        cty <- lift $ lookupDataConstructor en
        rcty <- retype cty
        addc rcty t
        return t
    constrain v@(VarE (Sig n t) _) = do
        tenv <- gets ti_tenv
        case lookup n tenv of
            Just t' -> addc t' t
            Nothing -> do
                ne <- enved n
                vt <- lift $ lookupVarType ne
                rvt <- retype vt
                addc rvt t
        return t

-- Only constrains the body. Doesn't constrain the patterns.
instance Constrain Match where
    constrain (Match p e) = scoped (bindingsP p) (constrain e)

instance Constrain Pat where
    constrain (ConP t n ps) = do
        tps <- mapM constrain ps
        en <- enved n
        cty <- lift $ lookupDataConstructor en
        rcty <- retype cty
        addc rcty (arrowsT (tps ++ [t]))
        let pts = init (unarrowsT rcty)
        sequence_ [addc pt tp | (pt, tp) <- zip pts tps]
        return t
    constrain (VarP (Sig _ t)) = return t
    constrain (IntegerP {}) = return integerT
    constrain (WildP t) = return t


-- Given a type, return a new version of the type with new VarTs.
retype :: Type -> TI Type
retype t = do
    (t', _) <- runStateT (retype' t) []
    return t'
       where
    namefor :: Name -> StateT [(Name, Name)] TI Name
    namefor n = do
        names <- get
        case lookup n names of
            Nothing -> do
                n' <- lift newvtn
                put $ (n, n') : names
                return n'
            Just n' -> return n'
    
    retype' :: Type -> StateT [(Name, Name)] TI Type 
    retype' t@(ConT {}) = return t
    retype' t@(AppT a b) = do
        a' <- retype' a
        b' <- retype' b
        return $ AppT a' b'
    retype' (VarT n) = do
        n' <- namefor n
        return (VarT n')
    retype' (ForallT _ _ t) = retype' t
    retype' UnknownT = return UnknownT

-- If the given type is in the map, replace it, otherwise keep it unchanged.
replace :: (Data a) => [(Type, Type)] -> a -> a
replace m =
    let base :: Type -> Type
        base t =
            case lookup t m of
                Just t' -> t'
                Nothing -> t
    in everywhere $ mkT base
            

-- Update any UnknownVI's in the given declarations.
-- This should be performed after type check, so the proper instance can be
-- chosen.
varize :: [Dec] -> [Dec]
varize ds = map varizedec ds
  where
    varizedec :: Dec -> Dec
    varizedec (ValD s e) = ValD s (varizeexp [] e)
    varizedec d@(DataD {}) = d
    varizedec d@(ClassD {}) = d
    varizedec d@(InstD cls ms) = InstD cls (map varizemeth ms)

    varizemeth :: Method -> Method
    varizemeth (Method n e) = Method n (varizeexp [] e)

    varizeexp :: [Name] -> Exp -> Exp
    varizeexp _ e@(IntegerE {}) = e
    varizeexp _ e@(PrimE {}) = e
    varizeexp bound (CaseE e ms) = CaseE (varizeexp bound e) (map (varizematch bound) ms)
    varizeexp bound (AppE a b) = AppE (varizeexp bound a) (varizeexp bound b)
    varizeexp bound (LamE (Sig n t) e) = LamE (Sig n t) (varizeexp (n : bound) e)
    varizeexp _ e@(ConE {}) = e
    varizeexp bound (VarE (Sig n t) _) | n `elem` bound = VarE (Sig n t) Bound
    varizeexp _ (VarE s _) = VarE s (lookupVarInfo (mkenv ds s))

    varizematch :: [Name] -> Match -> Match
    varizematch bound (Match p e) = Match p (varizeexp ((map fst (bindingsP p)) ++ bound) e)
        
    


