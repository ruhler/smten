
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Prelude ( 
   S.Bool(..), S.__caseTrue, S.__caseFalse,
   IO, Char(Char), Integer(Integer),
     ) where

import qualified Prelude
import Prelude hiding (IO, Char, Integer)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S
import Smten.Runtime.Haskelly

data Char = Char Prelude.Char
          | CharMux__ S.Bool Char Char

instance Haskelly Prelude.Char Char where
   frhs = Char
   tohs (Char c) = return c
   tohs _ = Nothing

instance SmtenHS0 Char where
   mux0 = CharMux__
   realize0 m c = 
      case c of
         Char {} -> c
         CharMux__ p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)

data Integer = Integer Prelude.Integer
             | IntegerMux__ S.Bool Integer Integer

instance Haskelly Prelude.Integer Integer where
   frhs = Integer
   tohs (Integer c) = return c
   tohs _ = Nothing

instance SmtenHS0 Integer where
   mux0 = IntegerMux__
   realize0 m c = 
      case c of
         Integer {} -> c
         IntegerMux__ p a b -> __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)

data IO a = IO (Prelude.IO a)
          | IOMux__ S.Bool (IO a) (IO a)

instance (Haskelly ha sa) => Haskelly (Prelude.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)
    tohs (IO x) = return (tohs' <$> x)
    tohs _ = Nothing

instance SmtenHS1 IO where
    mux1 = IOMux__
    realize1 m (IO x) = IO (realize0 m <$> x)
    realize1 m (IOMux__ p a b) = __caseTrue (realize0 m p) (realize0 m a) (realize0 m b)

