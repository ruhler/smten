
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Char (
    Char(Char),
    ) where

import Prelude hiding (Char)
import qualified Prelude as P
import Smten.Runtime.SmtenHS as S

data Char = Char P.Char
          | CharMux__ S.Bool Char Char

instance Haskelly P.Char Char where
   frhs = Char
   tohs (Char c) = c
   tohs _ = error "tohs.Char failed"

instance SmtenHS0 Char where
   mux0 = CharMux__
   realize0 m c = 
      case c of
         Char {} -> c
         CharMux__ p a b -> S.__caseTrue (realize0 m p) (realize0 m a) (realize0 m b)
   strict_app0 f (CharMux__ p a b) = mux0 p (strict_app0 f a) (strict_app0 f b)
   strict_app0 f c = f c


