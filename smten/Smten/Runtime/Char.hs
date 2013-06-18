
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.Char (
    Char(Char),
    ) where

import Prelude hiding (Char)
import qualified Prelude as P
import Smten.Runtime.SmtenHS as S

data Char = Char P.Char
          | Char_Prim (Assignment -> Char) Char
          | Char_Ite S.Bool Char Char
          | Char_Error ErrorString

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
         Char_Ite p a b -> __caseTrue (realize m p) (realize m a) (realize m b)
         Char_Prim r _ -> r m
   primitive0 = Char_Prim
   sapp0 f x =
     case x of
       Char {} -> f x
       Char_Ite p a b -> ite p (sapp f a) (sapp f b)
       Char_Error msg -> error0 msg
       _ -> error "TODO: sapp0 for symbolic Char"

   error0 = Char_Error
   ite0 = Char_Ite

