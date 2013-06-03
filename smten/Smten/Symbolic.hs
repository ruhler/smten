
module Smten.Symbolic (Solver(..)) where

data Solver = Yices1 | Yices2 | STP
            | Debug FilePath Solver
            | DebugLL FilePath Solver
    deriving (Show)

