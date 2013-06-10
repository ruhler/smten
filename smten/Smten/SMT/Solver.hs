
module Smten.SMT.Solver (Result(..), Solver(..)) where

import Data.Dynamic

data Result
    = Satisfiable
    | Unsatisfiable
    deriving (Eq, Show)

data Solver = Solver {
    -- | Declare free variables with the given name and type.
    declare_bool :: String -> IO (),
    declare_integer :: String -> IO (),
    declare_bit :: String -> Integer -> IO (),

    -- | Build up SMT formulas.
    assert :: Dynamic -> IO (),
    bool :: Bool -> IO Dynamic,
    integer :: Integer -> IO Dynamic,
    bit :: Integer -> Integer -> IO Dynamic,
    var :: String -> IO Dynamic,
    
    ite_bool :: Dynamic -> Dynamic -> Dynamic -> IO Dynamic,
    
    ite_integer :: Dynamic -> Dynamic -> Dynamic -> IO Dynamic,
    eq_integer :: Dynamic -> Dynamic -> IO Dynamic,
    leq_integer :: Dynamic -> Dynamic -> IO Dynamic,
    add_integer :: Dynamic -> Dynamic -> IO Dynamic,
    sub_integer :: Dynamic -> Dynamic -> IO Dynamic,
    
    ite_bit :: Dynamic -> Dynamic -> Dynamic -> IO Dynamic,
    eq_bit :: Dynamic -> Dynamic -> IO Dynamic,
    leq_bit :: Dynamic -> Dynamic -> IO Dynamic,
    add_bit :: Dynamic -> Dynamic -> IO Dynamic,
    sub_bit :: Dynamic -> Dynamic -> IO Dynamic,
    mul_bit :: Dynamic -> Dynamic -> IO Dynamic,
    or_bit :: Dynamic -> Dynamic -> IO Dynamic,

    -- | Query for values of free variables in the model.
    getBoolValue :: String -> IO Bool,
    getIntegerValue :: String -> IO Integer,
    getBitVectorValue :: String -> Integer -> IO Integer,

    -- | Run (check) and return the result.
    check :: IO Result
}

