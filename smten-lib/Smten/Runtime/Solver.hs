
module Smten.Runtime.Solver (
    Solver, SolverInst(..), solverInstFromAST,
    ) where

import qualified Smten.Runtime.Types as S
import Smten.Runtime.Result
import Smten.Runtime.SmtenHS
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

