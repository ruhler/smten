
module Smten.SMT.Solver.Dynamic (ST.Result(..), Solver(..), dynsolver) where

import qualified Smten.Runtime.SmtenHS as S
import qualified Smten.SMT.Solver.Static as ST
import qualified Smten.SMT.Assert as A

data Solver = Solver {
    -- | Assert the given expression.
    assert :: S.Bool -> IO (),

    -- | Declare a free boolean variable with given name.
    declare_bool :: String -> IO (),
    declare_integer :: String -> IO (),
    declare_bit :: String -> Integer -> IO (),

    getBoolValue :: String -> IO Bool,
    getIntegerValue :: String -> IO Integer,
    getBitVectorValue :: String -> Integer -> IO Integer,

    -- | Run (check) and return the result.
    check :: IO ST.Result
}

dynsolver :: (ST.Solver ctx exp) => ctx -> Solver
dynsolver x = Solver {
    assert = A.assert x,
    declare_bool = ST.declare_bool x,
    declare_integer = ST.declare_integer x,
    declare_bit = ST.declare_bit x,
    getBoolValue = ST.getBoolValue x,
    getIntegerValue = ST.getIntegerValue x,
    getBitVectorValue = ST.getBitVectorValue x,
    check = ST.check x
}
    

