
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Prelude ( 
   S.Bool(..), S.__caseTrue, S.__caseFalse,
   S.Integer(..),
   IO, Char(Char),
     ) where

import qualified Prelude
import Prelude hiding (IO, Char, Integer)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S

data Char = Char Prelude.Char
          | CharMux__ S.Bool Char Char

instance Haskelly Prelude.Char Char where
   frhs = Char
   tohs (Char c) = c
   tohs _ = error "tohs.Char failed"

instance SmtenHS0 Char where
   mux0 = CharMux__
   realize0 m c = 
      case c of
         Char {} -> c
         CharMux__ p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
   strict_app0 f (CharMux__ p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
   strict_app0 f c = f c

data IO a = IO (Prelude.IO a)
          | IOMux__ S.Bool (IO a) (IO a)

instance (Haskelly ha sa) => Haskelly (Prelude.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)
    tohs (IO x) = tohs <$> x
    tohs _ = error "tohs.IO failed"

instance SmtenHS1 IO where
    mux1 = IOMux__
    realize1 m (IO x) = IO (realize0 m <$> x)
    realize1 m (IOMux__ p a b) = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
    strict_app1 f (IOMux__ p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
    strict_app1 f x = f x

