
module Smten.Runtime.Result (Result(..)) where

data Result = Unsat | Sat
  deriving (Eq, Show)

