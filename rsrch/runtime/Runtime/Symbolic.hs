
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Runtime.Symbolic (
    Symbolic, run_symbolic,
    ) where


import Control.Monad.State

import qualified Smten.SMT.Solver as SMT
import Smten.SMT.Yices.Yices2
import qualified Smten.SMT.AST as AST
import Smten.Name

import qualified Runtime.Prelude as R


data SS = SS {
    ss_solver :: SMT.Solver,
    ss_pred :: R.S__ R.Bool,
    ss_free :: [Name],
    ss_formula :: R.S__ R.Bool
}

type Symbolic = StateT SS IO

instance R.Monad Symbolic where
    return = return_symbolic
    (>>=) = bind_symbolic

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

run_symbolic :: Symbolic (R.S__ a) -> R.IO (R.Maybe a)
run_symbolic q = do
    s <- yices2
    (x, ss) <- runStateT q (SS s (R.S__c R.True) [] (R.S__c R.True))
    assert s (ss_formula ss)
    res <- SMT.check s
    case res of
        SMT.Satisfiable -> do
            let vars = ss_free ss
            vals <- mapM (getvalue s) vars
            return (R.Just (transform (zip vars vals) x))
        SMT.Unsatisfiable -> return R.Nothing
    
assert :: SMT.Solver -> R.S__ R.Bool -> IO ()
assert = error "todo: assert"

getvalue :: SMT.Solver -> Name -> IO (R.Bool)
getvalue s nm = do
    v <- SMT.getBoolValue s nm
    return $ if v then R.True else R.False

transform :: [(Name, R.Bool)] -> R.S__ a -> a
transform m =
  let f :: R.S__ a -> a
      f (R.S__c x) = x
      f (R.S__if p a b) = 
        case f p of
            R.True -> f a
            R.False -> f b
  in f

