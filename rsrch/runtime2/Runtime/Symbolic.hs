
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Runtime.Symbolic (
    Symbolic, run_symbolic, fail_symbolic,
    free_Bool,
    ) where

import Control.Monad.State
import Data.Functor((<$>))

import Runtime.Yices2
import Smten.Name

import qualified Runtime.Prelude as R


data SS = SS {
    ss_solver :: Solver,
    ss_pred :: R.Bool,
    ss_free :: [Name],
    ss_formula :: R.Bool
}

type Symbolic = StateT SS IO

instance (R.SmtenHS a) => R.SmtenHS (Symbolic a) where
    mux p a b = do
        va <- predicated p a
        vb <- predicated (R.not p) b
        return (R.mux p va vb)

    realize m x = R.realize m <$> x

predicated :: R.Bool -> Symbolic a -> Symbolic a
predicated p q = do
    pold <- gets ss_pred
    modify $ \ss -> ss { ss_pred = pold R.&& p }
    v <- q
    modify $ \ss -> ss { ss_pred = pold }
    return v
        
instance R.Monad Symbolic where
    return = return_symbolic
    (>>=) = bind_symbolic

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

fail_symbolic :: Symbolic a
fail_symbolic = do
    modify $ \ss -> ss { ss_formula = ss_formula ss R.&& R.not (ss_pred ss) }
    return (error "fail_symbolic")

run_symbolic :: (R.SmtenHS a) => Symbolic a -> R.IO (R.Maybe a)
run_symbolic q = R.IO $ do
    s <- yices2
    (x, ss) <- runStateT q (SS s R.True [] R.True)
    assert s (ss_formula ss)
    res <- check s
    case res of
        Satisfiable -> do
            let vars = ss_free ss
            vals <- mapM (getBoolValue s) vars
            return (R.Just (R.realize (zip vars vals) x))
        Unsatisfiable -> return R.Nothing

free_Bool :: Symbolic R.Bool
free_Bool = do
    s <- gets ss_solver
    nm <- liftIO $ fresh_bool s
    modify $ \ss -> ss { ss_free = nm : ss_free ss }
    return (R.BoolVar nm)

