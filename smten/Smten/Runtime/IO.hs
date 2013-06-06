
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.IO (IO) where

import qualified Prelude as P
import Prelude hiding (IO)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S


data IO a = IO (P.IO a)
          | IOMux__ S.Bool (IO a) (IO a)

instance (Haskelly ha sa) => Haskelly (P.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)
    tohs (IO x) = tohs <$> x
    tohs _ = error "tohs.IO failed"

instance SmtenHS1 IO where
    mux1 = IOMux__
    realize1 m (IO x) = IO (realize0 m <$> x)
    realize1 m (IOMux__ p a b) = S.__caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
    strict_app1 f (IOMux__ p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
    strict_app1 f x = f x


