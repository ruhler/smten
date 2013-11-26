
module Smten.Runtime.Solver (
    Solver(..),
    solverFromAST,
    ) where

import qualified Smten.Runtime.Types as S
import Smten.Runtime.Bit
import Smten.Runtime.Model
import Smten.Runtime.Result
import Smten.Runtime.FreeID
import Smten.Runtime.SmtenHS
import qualified Smten.Runtime.SolverAST as AST
import qualified Smten.Runtime.Assert as A

newtype Solver = Solver {
    -- | Use a solver to solve a single SMT query.
    -- solve s vars formula
    --  s - the solver to use
    --  formula - the formula for the query
    -- Returns: Just a model if the query is satisfiable,
    --          Nothing if the query is unsatisfiable
    --   The model contains values for all user-level variables that appear in
    --   the formula.
    solve :: S.Bool -> IO (Maybe Model)
}

-- TODO: why do we need this?
instance SmtenHS0 Solver where
  ite0 = error "TODO: Solver.ite0"
  realize0 = error "TODO: Solver.realize0"
  

solverFromAST :: (AST.SolverAST ctx exp) => IO ctx -> Solver
solverFromAST mksolver = Solver $ \formula -> do
    solver <- mksolver
    vars <- A.assert solver formula
    res <- AST.check solver
    case res of 
        Sat -> do
            vals <- mapM (getValue solver) vars
            m <- model $ zip (map fst vars) vals
            AST.cleanup solver
            return (Just m)
        Unsat -> do
            AST.cleanup solver
            return Nothing


getValue :: (AST.SolverAST ctx exp) => ctx -> (FreeID, S.Type) -> IO Any
getValue s (f, S.BoolT) = do
   b <- AST.getBoolValue s (freenm f)
   return (BoolA $ if b then S.True else S.False)
getValue s (f, S.IntegerT) = do
   b <- AST.getIntegerValue s (freenm f)
   return (IntegerA $ S.Integer b)
getValue s (f, S.BitT w) = do
   b <- AST.getBitVectorValue s w (freenm f)
   return (BitA $ bv_make w b)

