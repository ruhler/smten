
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    P.Char, P.Int, P.Integer,
    List__(..), Tuple2__(..), Tuple3__(..), Tuple4__(..), Unit__(..), 
    error, undefined,

    fromList__, toList__,
 )  where

import qualified Prelude as P
import Smten.Runtime.ErrorString
import Smten.Runtime.SmtenHS

import Smten.Compiled.Smten.Smten.List
import Smten.Compiled.Smten.Smten.Tuple
import Smten.Compiled.Smten.Smten.Unit

fromList__ :: List__ a -> [a]
fromList__ Nil__ = []
fromList__ (Cons__ x xs) = x : fromList__ xs

toList__ :: [a] -> List__ a
toList__ [] = Nil__
toList__ (x:xs) = Cons__ x (toList__ xs)

error :: (SmtenHS0 a) => List__ P.Char -> a
error msg = error0 (errstr (fromList__ msg))

-- TODO: this function should not be specified manually, it should be
-- auto-generated.
undefined :: (SmtenHS0 a) => a
undefined = error0 (errstr "Prelude.undefined")

instance SmtenHS2 (->) where
    error2 msg = \x -> error0 msg
    realize2 m f = \x -> realize m (f (realize m x))
    ite2 p fa fb = \x -> ite p (fa x) (fb x)
    primitive2 r f = \x -> primitive0 (\m -> r m x) (f x)

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

instance SmtenHS1 P.IO where
    error1 msg = doerr msg
    realize1 = P.error "TODO: P.IO.realize1"
    ite1 = P.error "TODO: P.IO.ite1"

