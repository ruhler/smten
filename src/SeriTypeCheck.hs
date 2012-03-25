
module SeriTypeCheck (
    typecheck, typeinfer
    ) where

import Seri
import SeriPrint

import Control.Monad.State


-- Typecheck an expression.
-- All the type information should already be in the expression, this just
-- checks that that information is consistent.
--
-- It fails with a hopefully useful message if typecheck fails. Otherwise it
-- returns the type of the expression.
typecheck :: (Monad m) => Exp -> m Type
typecheck (IntegerE _) = return IntegerT

typecheck (AddE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return IntegerT

typecheck (MulE a b) = do
    at <- typecheck a
    typeassert IntegerT at a

    bt <- typecheck b
    typeassert IntegerT bt b

    return IntegerT

typecheck (AppE t f x) = do
    ft <- typecheck f
    xt <- typecheck x
    case ft of
        ArrowT at bt -> do
            typeassert at xt x
            return bt
        _ -> typefail "function" ft f
typecheck (LamE t@(ArrowT at bt) n body) = do
    bodyt <- typecheck body
    checkvars body n at
    typeassert bt bodyt body
    return t

typecheck e@(LamE t n body) = typefail "function" t e
typecheck (VarE t _) = return t

-- Check that all variables with the given name in the expression have the
-- given type. If they don't, fail with a hopefully meaningful message.
checkvars :: Monad m => Exp -> Name -> Type -> m ()
checkvars (IntegerE _) _ _ = return ()
checkvars (AddE a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (MulE a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (AppE _ a b) n v = do
    checkvars a n v
    checkvars b n v
checkvars (LamE _ ln body) n v | ln /= n = checkvars body n v
checkvars (LamE _ ln body) n v = return ()
checkvars e@(VarE t vn) n v | vn == n = typeassert v t e
checkvars e@(VarE t vn) n v = return ()


-- typefail expected found expr
-- Indicate a type failure.
typefail :: (Monad m) => String -> Type -> Exp -> m a
typefail exp fnd expr
  = fail $ "Expected type " ++ exp ++ ", found type " ++ show fnd
            ++ " in the expression " ++ show (ppr expr)

typeassert :: (Monad m) => Type -> Type -> Exp -> m ()
typeassert exp fnd expr
  = if (exp == fnd)
        then return ()
        else typefail (show $ ppr exp) fnd expr
  

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
tereplace _ e@(IntegerE _) = e
tereplace l (AddE a b) = AddE (tereplace l a) (tereplace l b)
tereplace l (MulE a b) = MulE (tereplace l a) (tereplace l b)
tereplace l (AppE t a b) =
    let a' = tereplace l a
        b' = tereplace l b
    in case (lookup t l) of
          Just t' -> AppE t' a' b'
          Nothing -> AppE t a' b'
tereplace l (LamE t n e) =
    let e' = tereplace l e
    in case (lookup t l) of
          Just t' -> LamE t' n e'
          Nothing -> LamE t n e'
tereplace l (VarE t n) =
    case (lookup t l) of
          Just t' -> VarE t' n
          Nothing -> VarE t n


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

