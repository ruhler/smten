
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    P.Char, P.Int, P.Integer,
    List__(..), Tuple2__(..), Tuple3__(..), Tuple4__(..), Unit__(..), 
    error, undefined,
 )  where

import qualified Prelude as P
import Smten.Runtime.ErrorString
import Smten.Runtime.SmtenHS

import Smten.Compiled.Smten.Smten.List
import Smten.Compiled.Smten.Smten.Tuple
import Smten.Compiled.Smten.Smten.Unit

error :: (SmtenHS0 a) => List__ P.Char -> a
error msg = 
  let getstr (Nil__) = []
      getstr (Cons__ x xs) = x : getstr xs
  in error0 (errstr (getstr msg))

-- TODO: this function should not be specified manually, it should be
-- auto-generated.
undefined :: (SmtenHS0 a) => a
undefined = error0 (errstr "Prelude.undefined")

instance SmtenHS0 P.Char where
    error0 = P.error "TODO: Char.error0"
    realize0 = P.error "TODO: Char.realize0"
    ite0 = P.error "TODO: Char.ite0"

instance SmtenHS0 P.Int where
    error0 = P.error "TODO: Int.error0"
    realize0 = P.error "TODO: Int.realize0"
    ite0 = P.error "TODO: Int.ite0"

instance SmtenHS0 P.Integer where
    error0 = P.error "TODO: Integer.error0"
    realize0 = P.error "TODO: Integer.realize0"
    ite0 = P.error "TODO: Integer.ite0"

