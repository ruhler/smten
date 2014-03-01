
module Smten.Runtime.Solver (
    Solver(..),
    solverFromAST,
    ) where

import Smten.Runtime.Model
import Smten.Runtime.Result
import Smten.Runtime.Formula.Finite
import Smten.Runtime.SmtenHS
import Smten.Runtime.SolverAST
import Smten.Runtime.Build

newtype Solver = Solver {
    -- | Use a solver to solve a single SMT query.
    -- solve s vars formula
    --  s - the solver to use
    --  formula - the formula for the query
    -- Returns: Just a model if the query is satisfiable,
    --          Nothing if the query is unsatisfiable
    --   The model contains values for all user-level variables that appear in
    --   the formula.
    solve :: BoolFF -> IO (Maybe Model)
}

-- TODO: why do we need this?
instance SmtenHS0 Solver where
  ite0 = error "TODO: Solver.ite0"
  unreachable0 = error "TODO: Solver.unreachable0"
  

{-# INLINEABLE solverFromAST #-}
solverFromAST :: (SolverAST ctx b i v) => IO ctx -> Solver
solverFromAST mksolver = Solver $ \formula -> do
    solver <- mksolver
    (p, vars) <- {-# SCC "Build" #-} build solver formula
    {-# SCC "Assert" #-} assert solver p
    res <- {-# SCC "Check" #-} check solver
    case res of 
        Sat -> do
            vals <- {-# SCC "GetModel" #-} getModel solver vars
            m <- model $ zip (map fst vars) vals
            {-# SCC "Cleanup" #-} cleanup solver
            return (Just m)
        Unsat -> do
            {-# SCC "Cleanup" #-} cleanup solver
            return Nothing

