
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    Integer(..),
    error,
    fromList__, toHSChar, toHSString, fromHSString,
    module Smten.Compiled.Smten.Smten.Char,
    module Smten.Compiled.Smten.Smten.Int,
    module Smten.Compiled.Smten.Smten.List,
    module Smten.Compiled.Smten.Smten.Tuple,
    module Smten.Compiled.Smten.Smten.Unit,
 )  where

import qualified Prelude as P
import qualified GHC.Types as P

import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

import Smten.Compiled.Smten.Smten.Char
import Smten.Compiled.Smten.Smten.Int
import Smten.Compiled.Smten.Smten.List
import Smten.Runtime.Char
import Smten.Runtime.Int
import Smten.Runtime.Integer
import Smten.Compiled.Smten.Smten.Tuple
import Smten.Compiled.Smten.Smten.Unit

instance (SmtenHS0 a) => SymbolicOf [a] (List__ a) where
    tosym [] = __Nil__
    tosym (x:xs) = __Cons__ x (tosym xs)

    symapp f x = ite0 (gdNil__ x) (f []) (symapp (\xsl -> f ((fl1Cons__ x) : xsl)) (fl2Cons__ x))

instance SymbolicOf [P.Char] (List__ Char) where
    tosym [] = __Nil__
    tosym (x:xs) = __Cons__ (tosym x) (tosym xs)

    symapp f x = ite0 (gdNil__ x) (f []) (symapp2 (\xv xsv -> f (xv : xsv)) (fl1Cons__ x) (fl2Cons__ x))
                    
fromList__ :: List__ a -> [a]
fromList__ x
  | isTrueF (gdNil__ x) = []
  | isTrueF (gdCons__ x) = fl1Cons__ x : fromList__ (fl2Cons__ x)

error :: (SmtenHS0 a) => List__ Char -> a
error msg = {-# SCC "PRIM_ERROR" #-} P.error (toHSString msg)

-- Because there is no way to make use of an IO computation in constructing a
-- formula, it doesn't matter what we do with ite1 and unreachable1 when p is
-- not concrete.
instance SmtenHS1 P.IO where
  ite1 p a b = iteS p a b (P.return unreachable)
  unreachable1 = P.return unreachable

instance SymbolicOf P.Char Char where
   tosym (P.C# v) = __C# (char# v)
   symapp f c =
     let g v = f (P.C# v)
     in primCharApp g (fl1C# c)

instance SymbolicOf P.Int Int where
   tosym (P.I# v) = __I# (int# v)
   symapp f i =
     let g v = f (P.I# v)
     in primIntApp g (fl1I# i)

toHSChar :: Char -> P.Char
toHSChar c = P.C# (toHSChar# (fl1C# c))

toHSString :: List__ Char -> P.String
toHSString x = P.map toHSChar (fromList__ x)

fromHSString :: P.String -> List__ Char
fromHSString x = tosym (P.map tosym x :: [Char])

