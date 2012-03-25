
module Seri.TypeInfer (
    typeinfer
    ) where

import Control.Monad.State

import Seri.IR

-- typeinfer 
--  Run typeinference on an expression.
--  Types marked TyUnknown are inferred.
--  Assumes there are no VarT's in the expression.
--  The returned expression has types inferred, but they may be incorrectly
--  inferred if the expression doesn't type check, so you should run typecheck
--  after inference to make sure it's valid.
typeinfer :: Exp -> Exp
typeinfer eorig
 = let (evared, cons) = constraints eorig
       sol = solve cons
   in tereplace sol evared

-- replace each type in expression according to the given association list.
tereplace :: [(Type, Type)] -> Exp -> Exp
tereplace l = traverse $ Traversal {
    tr_int = \e _ -> e,
    tr_add = \_ -> AddE,
    tr_mul = \_ -> MulE,
    tr_app = \_ t ->
        case (lookup t l) of
            Just t' -> AppE t'
            Nothing -> AppE t,
    tr_lam = \_ t ->
        case (lookup t l) of
            Just t' -> LamE t'
            Nothing -> LamE t,
    tr_var = \_ t ->
        case (lookup t l) of
            Just t' -> VarE t'
            Nothing -> VarE t
}

-- Replace all unknown types with variable types.
-- State is the id of the next free type variable to use.
ununknown :: Exp -> State Integer Exp
ununknown e@(IntegerE _) = return e
ununknown (AddE a b) = do
    a' <- ununknown a
    b' <- ununknown b
    return $ AddE a' b'
ununknown (MulE a b) = do
    a' <- ununknown a
    b' <- ununknown b
    return $ MulE a' b'
ununknown (AppE t a b) = do
    t' <- ununknownt t
    a' <- ununknown a
    b' <- ununknown b
    return $ AppE t' a' b'
ununknown (LamE t n b) = do
    t' <- ununknownt t  
    b' <- ununknown b
    return $ LamE t' n b'
ununknown (VarE t n) = do
    t' <- ununknownt t
    return $ VarE t' n

ununknownt :: Type -> State Integer Type
ununknownt t@IntegerT = return t
ununknownt (ArrowT a b) = do
    a' <- ununknownt a
    b' <- ununknownt b
    return $ ArrowT a' b'
ununknownt UnknownT = do
    id <- get
    put (id+1)
    return $ VarT id
ununknownt t@(VarT _) = error $ "Found VarT while ununknownting"

constraints :: Exp -> (Exp, [(Type, Type)])
constraints e
 = let (vared, nid) = runState (ununknown e) 0
       (_, (_, cs)) = runState (constrain vared) (nid, [])
   in (vared, cs)

-- Generate type constraints for an expression, assuming no UnknownT types are
-- in it.
constrain :: Exp -> State (Integer, [(Type, Type)]) ()
constrain (IntegerE _) = return ()
constrain (AddE a b) = do
    constrain a
    constrain b
    addc IntegerT (typeof a)
    addc IntegerT (typeof b)
constrain (MulE a b) = do
    constrain a
    constrain b
    addc IntegerT (typeof a)
    addc IntegerT (typeof b)
constrain (AppE t f x) = do
    constrain f
    constrain x
    it <- nextv
    ot <- nextv
    addc (ArrowT it ot) (typeof f)
    addc ot t
    addc it (typeof x)
constrain (LamE t n b) = do
    constrain b
    it <- nextv
    ot <- nextv
    addc (ArrowT it ot) t
    addc ot (typeof b)
    constrainvs b n it
constrain (VarE _ _) = return ()

constrainvs :: Exp -> Name -> Type -> State (Integer, [(Type, Type)]) ()
constrainvs (IntegerE _) _ _ = return ()
constrainvs (AddE a b) n v = do
    constrainvs a n v
    constrainvs b n v
constrainvs (MulE a b) n v = do
    constrainvs a n v
    constrainvs b n v
constrainvs (AppE _ a b) n v = do
    constrainvs a n v
    constrainvs b n v
constrainvs (LamE _ nm _) n _ | nm == n = return ()
constrainvs (LamE _ _ b) n v = constrainvs b n v
constrainvs (VarE _ nm) n _ | nm /= n = return ()
constrainvs (VarE t _) _ v = addc t v

addc :: Type -> Type -> State (Integer, [(Type, Type)]) ()
addc a b = do
    (i, cs) <- get  
    put (i, (a, b):cs)

nextv :: State (Integer, [(Type, Type)]) Type
nextv = do
    (i, cs) <- get
    put (i+1, cs)
    return (VarT i)


-- Solve a type constraint system.
--
-- Here's how we solve it:
--    We define an order on Types based on how well known they are. So
--    IntegerT, ArrowT, etc... are very well known. VarT less so, and we say
--    VarT 4 is less known than VarT 1, for instance.
--
--    For any constraint of the form X = Y, we use that to replace every
--    occurence of the less well known type with the more well known type. For
--    example, say Y is less well known. Every occurence of Y in all the
--    constraints is replaced with X, and we add Y = X to the solution set.
--
--    The claim is, after going through each constraint, we are left with
--    the best known definitions of each lesser known type we can find.
--
-- This ignores inconsistent constraints. We'll let the typechecker catch
-- those when checking the solved system, because it can give better error
-- messages.
solve' :: State ([(Type, Type)], [(Type, Type)]) [(Type, Type)]
solve' = do
    (ins, outs) <- get
    case ins of
        [] -> return outs
        (x:xs) -> do
            put (xs, outs)
            solveconstraint x
            solve'

solve :: [(Type, Type)] -> [(Type, Type)]
solve xs = fst $ runState solve' (xs, [])
        
solveconstraint :: (Type, Type) -> State ([(Type, Type)], [(Type, Type)]) ()
solveconstraint (x, y) | x == y = return ()
solveconstraint ((ArrowT a b), (ArrowT c d))
  = solveconstraint (a, c) >> solveconstraint (b, d)
solveconstraint (a, b) | b `lessknown` a = solveconstraint (b, a)
solveconstraint (a, b) = do
    (ins, outs) <- get
    let ins' = map (tpreplace a b) ins
    let outs' = map (tpreplace a b) outs
    put (ins', (a, b):outs')

tpreplace :: Type -> Type -> (Type, Type) -> (Type, Type)
tpreplace k v (a, b) = (treplace k v a, treplace k v b)

-- treplace k v x
-- Replace every occurence of type k in x with type v.
treplace :: Type -> Type -> Type -> Type
treplace k v x | k == x = v
treplace k v (ArrowT a b) = ArrowT (treplace k v a) (treplace k v b)
treplace _ _ x = x 

lessknown :: Type -> Type -> Bool
lessknown (VarT a) (VarT b) = a > b
lessknown (VarT _) IntegerT = True
lessknown (VarT _) (ArrowT _ _) = True
lessknown UnknownT _ = error $ "UnknownT found in lessknown"
lessknown _ UnknownT = error $ "UnknownT found in lessknown"
lessknown a b = False

