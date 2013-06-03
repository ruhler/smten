
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Runtime.Symbolic (
    Symbolic, return_symbolic, bind_symbolic, run_symbolic,
    IO, Maybe, Solver,
    ) where

import Control.Monad.State
import Data.Functor((<$>))

import Smten.Symbolic
import Smten.Runtime.Haskelly
import qualified Smten.Runtime.Prelude as R

type FID = Integer

data SS = SS {
    ss_pred :: R.Bool,
    ss_free :: [FID],
    ss_formula :: R.Bool
}

type Symbolic = State SS

instance (Haskelly ha sa) => Haskelly (Symbolic ha) (Symbolic sa) where
    frhs x = frhs <$> x
    tohs x = return (tohs' <$> x)

return_symbolic :: a -> Symbolic a
return_symbolic = return

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic = (>>=)

run_symbolic :: Solver -> Symbolic a -> IO (Maybe a)
run_symbolic s q = return Nothing

