
module Smten.SMT.Solver (Result(..), Solver(..)) where

import qualified Smten.Runtime.Prelude as R

data Result
    = Satisfiable
    | Unsatisfiable
    deriving (Eq, Show)

data Solver = Solver {
    -- | Assert the given expression.
    assert :: R.Bool -> IO (),

    -- | Run (check) and return the result.
    check :: IO Result
}

