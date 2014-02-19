
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.GHC.Integer.Type (
   plusInteger, minusInteger, timesInteger,
   absInteger, signumInteger, eqInteger,
   leInteger, smallInteger,
    ) where

import GHC.Prim
import GHC.Types(Bool(..))
import qualified GHC.Integer as P
import Smten.Smten.Base
import Smten.Plugin.Annotations

{-# ANN module PrimitiveModule #-}

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger = P.plusInteger

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger = P.minusInteger

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger = P.timesInteger

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger = P.absInteger

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = P.signumInteger

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger = P.eqInteger

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger = P.leInteger

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger = P.smallInteger

