
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.IO (IO) where

import qualified Prelude as P
import Prelude hiding (IO)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S
import Smten.Runtime.Debug

data IO a = IO (P.IO a)
          | IO_Prim (Assignment -> IO a) (Cases (IO a)) Debug
          | IO_Error String

instance (Haskelly ha sa) => Haskelly (P.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)

    mtohs (IO x) = return (stohs <$> x)
    mtohs _ = Nothing

    stohs (IO x) = stohs <$> x
    stohs _ = error "stohs.IO failed"

instance SmtenHS1 IO where
    realize1 m (IO x) = IO (realize m <$> x)
    realize1 m (IO_Prim r _ _) = realize m (r m)

    cases1 x@(IO {}) = concrete x
    cases1 (IO_Prim _ c _) = c

    debug1 (IO {}) = dbgText "?IO?"
    debug1 (IO_Prim _ _ d) = d
    debug1 (IO_Error msg) = dbgError msg

    primitive1 = IO_Prim
    error1 = IO_Error

