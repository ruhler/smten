
module Seri.Lambda.TypeInfer (
    ) where

-- | Perform type inference on the given declaration.
-- Types UnknownT are inferred.
-- Variable info UnknownVI is inferred.
--
-- The returned expression may have incorrectly inferred types if the
-- expression doesn't type check, so you should run typecheck after inference
-- to make sure it's valid.
typeinfer :: Env Dec -> Dec


-- | Replace all UnknownT with new variable types.
-- State is the id of the next free type variable to use.
deunknown :: (Data e) => e -> State Integer e
deunknown =
    let ununt UnknownT = newvt
        ununt t = return t
    in everywhreM (mkM ununt) 


data TIS = TIS {
    ti_varid :: Integer,        -- ^ The next free VarT id
    ti_cons :: [(Type, Type)]   -- ^ A list of accumulated type constraints
    ti_env :: [(Name, Type)]    -- ^ Types of bound variables in scope
}

type TI = State TIS

-- | Add a type constraint
addc :: Type -> Type -> TI ()
addc a b = modify $ \ti -> ti { ti_cons = (a, b) : (ti_cons ti) }

-- | Return a new variable type.
-- State is the id of the next free type variable to use.
newvt :: TI Type
newvt = do
    id <- gets ti_varid
    modify $ \ti -> ti { ti_varid = (id + 1) }
    return $ VarT ("_tc_" ++ show id)

-- | Run type checking with additional bound variable's in scope.
scoped :: [(Name, Type)] -> TI a -> TI a
scoped vars x = do
    env <- gets ti_env
    modify $ \ti -> { ti_env = vars ++ env }
    r <- x
    modify $ \ti -> { ti_env = env }
    return r

class Constrain a where
    -- | Generate type constraints for an expression, assuming no UnknownT types
    -- are in it.
    constrain :: a -> TI ()

instance Constrain Exp where
    constrain (IntegerE {}) = return ()
    constrain (PrimE {}) = return ()
    constrain (CaseE e ms) = do
        constrain e
        mapM constrain ms
        sequence [addc (typeof e) (typeof p) | Match p _ <- ms]
        sequence [addc (typeof (head ms)) (typeof m) | m <- ms]
    constrain (AppE f x) = do
        constrain f
        constrain x
        it <- newvt
        ot <- newvt
        addc (arrowsT [it, ot]) (typeof f)
        addc it (typeof x)
    constrain (LamE (Sig n t) b) = do
        constrain b
        scoped [(n, t)] (constrain b)
    constrain (ConE (Sig n t)) = do
        case lookupDataConstructor

instance Constrain Match where
    constrain (Match p e) = do
        constrain p
        scoped (bindingsP p) (constrain e)

