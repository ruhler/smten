
module Runtime.Symbolic (
    Symbolic, run_symbolic
    ) where

import qualified Runtime.Prelude as R

data Symbolic a

instance R.Monad Symbolic where
    return = error "TODO: return_symbolic"
    (>>=) = error "TODO: bind_symbolic"

run_symbolic :: Symbolic R.Unit__ -> R.IO (R.Maybe R.Unit__)
run_symbolic _ = R.return (R.Just R.Unit__)

