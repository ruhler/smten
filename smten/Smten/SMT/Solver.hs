
module Smten.SMT.Solver (Result(..), Solver(..)) where

import qualified Smten.Runtime.SmtenHS as S

data Result
    = Satisfiable
    | Unsatisfiable
    deriving (Eq, Show)

data Solver = Solver {
    -- | Assert the given expression.
    assert :: S.Bool -> IO (),

    -- | Declare a free boolean variable with given name.
    declare_bool :: String -> IO (),
    declare_integer :: String -> IO (),

    getBoolValue :: String -> IO Bool,
    getIntegerValue :: String -> IO Integer,

    -- | Run (check) and return the result.
    check :: IO Result
}

