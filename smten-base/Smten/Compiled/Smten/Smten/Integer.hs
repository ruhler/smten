

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Integer (
    Integer(..),
  ) where

import qualified Prelude as P

import Smten.Runtime.Types
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

instance SymbolicOf P.Integer Integer where
    tosym = Integer

    symapp f x =
      case x of
        Integer i -> f i
        Integer_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        Integer_Err msg -> error0 msg
        _ -> P.error "symapp on non-ite symbolic integer"

instance P.Num Integer where
    fromInteger = Integer
    (+) = P.error "Smten Integer P.Num (+) not supported"
    (*) = P.error "Smten Integer P.Num (*) not supported"
    abs = P.error "Smten Integer P.Num abs not supported"
    signum = P.error "Smten Integer P.Num signum not supported"

