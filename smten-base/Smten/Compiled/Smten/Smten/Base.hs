
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    Char(..), P.Int, Integer(..),
    List__(..), Tuple2__(..), Tuple3__(..), Tuple4__(..), Unit__(..), 
    error, undefined,

    fromList__, toList__, toHSChar, fromHSChar, toHSString, fromHSString,
 )  where

import qualified Prelude as P

import qualified GHC.Types as P
import qualified GHC.Prim as P

import Smten.Runtime.ErrorString
import Smten.Runtime.Formula
import Smten.Runtime.Model
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

error :: (SmtenHS0 a) => List__ Char -> a
error msg = error0 (errstr (toHSString msg))

-- TODO: this function should not be specified manually, it should be
-- auto-generated.
undefined :: (SmtenHS0 a) => a
undefined = error0 (errstr "Prelude.undefined")

instance SmtenHS2 (->) where
    error2 msg = \x -> error0 msg
    realize2 m f = \x -> realize m (f (realize m x))
    ite2 p fa fb = \x -> ite p (fa x) (fb x)
    primitive2 r f = \x -> primitive0 (\m -> r m x) (f x)

instance SmtenHS0 P.Int where
    error0 = P.error "TODO: Int.error0"
    realize0 = P.error "TODO: Int.realize0"
    ite0 = P.error "TODO: Int.ite0"

instance SmtenHS1 P.IO where
    error1 msg = doerr msg
    realize1 = P.error "TODO: P.IO.realize1"
    ite1 = P.error "TODO: P.IO.ite1"

data Char =
    C# P.Char#
  | Char_Ite BoolF Char Char
  | Char_Err ErrorString
  | Char_Prim (Model -> Char) Char

toHSChar :: Char -> P.Char
toHSChar (C# x) = P.C# x

toHSString :: List__ Char -> P.String
toHSString x = P.map toHSChar (fromList__ x)

fromHSString :: P.String -> List__ Char
fromHSString x = toList__ (P.map fromHSChar x)

fromHSChar :: P.Char -> Char
fromHSChar (P.C# x) = C# x

instance SmtenHS0 Char where
    error0 = Char_Err
    realize0 m x = 
      case x of
        C# {} -> x
        Char_Ite p a b -> iterealize p a b m
        Char_Err msg -> Char_Err (realize m msg)
        Char_Prim r _ -> r m
    ite0 = Char_Ite
    primitive0 = Char_Prim

data Integer =
    Integer P.Integer
  | Integer_Ite BoolF Integer Integer
  | Integer_Err ErrorString
  | Integer_Prim (Model -> Integer) Integer

instance SmtenHS0 Integer where
    error0 = Integer_Err
    realize0 m x =
      case x of
        Integer {} -> x
        Integer_Ite p a b -> iterealize p a b m
        Integer_Err msg -> Integer_Err (realize m msg)
        Integer_Prim r _ -> r m
    ite0 = Integer_Ite
    primitive0 = Integer_Prim

instance P.Num Integer where
    fromInteger = Integer
    (+) = P.error "Smten Integer P.Num (+) not supported"
    (*) = P.error "Smten Integer P.Num (*) not supported"
    abs = P.error "Smten Integer P.Num abs not supported"
    signum = P.error "Smten Integer P.Num signum not supported"

