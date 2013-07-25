
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    Char(..), Int(..), Integer(..),
    List__(..), Tuple2__(..), Tuple3__(..), Tuple4__(..), Unit__(..), 
    error,

    fromList__, toList__, toHSChar, toHSString, fromHSString,
 )  where

import qualified Prelude as P

import Smten.Runtime.ErrorString
import Smten.Runtime.Formula
import Smten.Runtime.Model
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

import Smten.Compiled.Smten.Smten.Char
import Smten.Compiled.Smten.Smten.Int
import Smten.Compiled.Smten.Smten.Integer
import Smten.Compiled.Smten.Smten.List
import Smten.Compiled.Smten.Smten.Tuple
import Smten.Compiled.Smten.Smten.Unit

fromList__ :: List__ a -> [a]
fromList__ Nil__ = []
fromList__ (Cons__ x xs) = x : fromList__ xs

toList__ :: [a] -> List__ a
toList__ [] = Nil__
toList__ (x:xs) = Cons__ x (toList__ xs)

error :: (SmtenHS0 a) => List__ Char -> a
error msg = error0 (errstr (toHSString msg))

instance SmtenHS1 P.IO where
    error1 msg = doerr msg
    realize1 = P.error "TODO: P.IO.realize1"
    ite1 = P.error "TODO: P.IO.ite1"

toHSString :: List__ Char -> P.String
toHSString x = P.map toHSChar (fromList__ x)

fromHSString :: P.String -> List__ Char
fromHSString x = toList__ (P.map tosym x)

