
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    Char(..), Int(..), Integer(..),
    List__(..), Tuple2__(..), Tuple3__(..), Tuple4__(..), Unit__(..), 
    error,

    fromList__, toHSChar, toHSString, fromHSString,
 )  where

import qualified Prelude as P

import Smten.Runtime.Types
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

import Smten.Compiled.Smten.Smten.Char
import Smten.Compiled.Smten.Smten.Int
import Smten.Compiled.Smten.Smten.Integer ()
import Smten.Compiled.Smten.Smten.List
import Smten.Compiled.Smten.Smten.Tuple
import Smten.Compiled.Smten.Smten.Unit

instance SymbolicOf [a] (List__ a) where
    tosym [] = Nil__
    tosym (x:xs) = Cons__ x (tosym xs)

    symapp f x =
      case x of
         Nil__ -> f []
         Cons__ x xs -> symapp (\xsl -> f (x:xsl)) xs
         List___Prim r m -> primitive0 (\m -> realize m (f $$ (r m))) (f $$ x)
         List___Error msg -> error0 msg
         List___Ite itenil itcon iteerr -> P.error "TODO: syammp List__Ite"

fromList__ :: List__ a -> [a]
fromList__ Nil__ = []
fromList__ (Cons__ x xs) = x : fromList__ xs

error :: (SmtenHS0 a) => List__ Char -> a
error msg = error0 (errstr (toHSString msg))

instance SmtenHS1 P.IO where
    error1 msg = doerr msg
    realize1 = P.error "TODO: P.IO.realize1"
    ite1 = P.error "TODO: P.IO.ite1"

toHSString :: List__ Char -> P.String
toHSString x = P.map toHSChar (fromList__ x)

fromHSString :: P.String -> List__ Char
fromHSString x = tosym (P.map tosym x :: [Char])

