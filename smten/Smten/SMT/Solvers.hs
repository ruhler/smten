
module Smten.SMT.Solvers (
    Solver(..), mkSolver
    ) where

import qualified Smten.SMT.Solver as SMT

import Smten.SMT.Yices1.Yices1
import Smten.SMT.Yices2.Yices2
import Smten.SMT.STP.STP
import Smten.SMT.DebugLL

data Solver = Yices1 | Yices2 | STP
            | Debug FilePath Solver
            | DebugLL FilePath Solver
    deriving (Show)

mkSolver :: Solver -> IO (SMT.Solver)
mkSolver Yices1 = yices1
mkSolver Yices2 = yices2
mkSolver STP = stp
mkSolver (DebugLL dbg s) = do
    s' <- mkSolver s
    debugll dbg s'
mkSolver d = error $ "TODO: mksolver: " ++ show d


