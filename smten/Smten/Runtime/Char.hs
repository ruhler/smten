
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Char (
    Char(Char),
    ) where

import Prelude hiding (Char)
import qualified Prelude as P
import Smten.Runtime.SmtenHS as S
import Smten.SMT.FreeID

data Char = Char P.Char
          | Char_Prim (Assignment -> Char) (Cases Char)

instance Haskelly P.Char Char where
   frhs = Char

   stohs (Char c) = c
   stohs _ = error "tohs.Char failed"

   mtohs (Char c) = return c
   mtohs _ = Nothing

instance SmtenHS0 Char where
   realize0 m c = 
      case c of
         Char {} -> c
         Char_Prim r _ -> r m

   cases0 c =
      case c of 
        Char {} -> concrete c
        Char_Prim _ x -> x

   primitive0 = Char_Prim

