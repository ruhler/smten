
module Smten.Runtime.Solver (
    Solver, solve,
    SolverInst(..), solverInstFromAST,
    ) where

import qualified Smten.Runtime.Types as S
import Smten.Runtime.Result
import Smten.Runtime.FreeID
import Smten.Runtime.SmtenHS
import Smten.Runtime.Bit
import qualified Smten.Runtime.SolverAST as AST
import qualified Smten.Runtime.Assert as A

type Solver = IO SolverInst

data SolverInst = SolverInst {
    -- | Assert the given expression.
    assert :: S.Bool -> IO (),

    -- | Declare a free variable with given name and type.
    declare :: S.Type -> String -> IO (),

    getBoolValue :: String -> IO Bool,
    getIntegerValue :: String -> IO Integer,
    getBitVectorValue :: Integer -> String -> IO Integer,

    -- | Run (check) and return the result.
    check :: IO Result,

    -- | Called after a query is complete.
    cleanup :: IO ()
}

-- TODO: why do we need this?
instance SmtenHS0 SolverInst where
  error0 = error "SolverInst.error0"
  ite0 = error "SolverInst.ite0"
  realize0 = error "SolverInst.realize0"

solverInstFromAST :: (AST.SolverAST ctx exp) => ctx -> SolverInst
solverInstFromAST x = SolverInst {
    assert = A.assert x,
    declare = AST.declare x,
    getBoolValue = AST.getBoolValue x,
    getIntegerValue = AST.getIntegerValue x,
    getBitVectorValue = AST.getBitVectorValue x,
    check = AST.check x,
    cleanup = AST.cleanup x
}

-- | Use a solver to solve a single SMT query.
-- solve s vars formula
--  s - the solver to use
--  vars - the free variables used in the query along with their types
--  formula - the formula for the query
-- Returns: Just a model if the query is satisfiable,
--          Nothing if the query is unsatisfiable
solve :: Solver -> [(FreeID, S.Type)] -> S.Bool -> IO (Maybe S.Model)
solve s vars formula = do
    solver <- s
    mapM_ (declVar solver) vars
    assert solver formula
    res <- check solver
    case res of 
        Sat -> do
            vals <- mapM (getValue solver) vars
            m <- S.model $ zip (map fst vars) vals
            cleanup solver
            return (Just m)
        Unsat -> do
            cleanup solver
            return Nothing


getValue :: SolverInst -> (FreeID, S.Type) -> IO S.Any
getValue s (f, S.BoolT) = do
   b <- getBoolValue s (freenm f)
   return (S.BoolA $ if b then S.True else S.False)
getValue s (f, S.IntegerT) = do
   b <- getIntegerValue s (freenm f)
   return (S.IntegerA $ S.Integer b)
getValue s (f, S.BitT w) = do
   b <- getBitVectorValue s w (freenm f)
   return (S.BitA $ bv_make w b)

declVar :: SolverInst -> (FreeID, S.Type) -> IO ()
declVar s (nm, ty) = declare s ty (freenm nm)

