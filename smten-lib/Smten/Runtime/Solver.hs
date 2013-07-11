
module Smten.Runtime.Solver (Solver(..), solverFromAST) where

import Smten.Runtime.Formula
import Smten.Runtime.Result
import qualified Smten.Runtime.SolverAST as AST
import qualified Smten.Runtime.Assert as A

data Solver = Solver {
    -- | Assert the given expression.
    assert :: BoolF -> IO (),

    -- | Declare a free variable with given name and type.
    declare :: TypeF -> String -> IO (),

    getBoolValue :: String -> IO Bool,

    -- | Run (check) and return the result.
    check :: IO Result
}

solverFromAST :: (AST.SolverAST ctx exp) => ctx -> Solver
solverFromAST x = Solver {
    assert = A.assert x,
    declare = AST.declare x,
    getBoolValue = AST.getBoolValue x,
    check = AST.check x
}
    

