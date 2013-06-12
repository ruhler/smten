
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.IO (IO) where

import qualified Prelude as P
import Prelude hiding (IO)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S
import Smten.SMT.FreeID 

data IO a = IO (P.IO a)
          | IO_Prim (Assignment -> IO a) (Cases (IO a))
          | IO_Error String

instance (Haskelly ha sa) => Haskelly (P.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)

    mtohs (IO x) = return (stohs <$> x)
    mtohs _ = Nothing

    stohs (IO x) = stohs <$> x
    stohs _ = error "stohs.IO failed"

instance SmtenHS1 IO where
    realize1 m (IO x) = IO (realize0 m <$> x)
    realize1 m (IO_Prim r _) = realize0 m (r m)

    cases1 x@(IO {}) = concrete x
    cases1 (IO_Prim _ c) = c

    primitive1 = IO_Prim
    error1 = IO_Error

