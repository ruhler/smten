
{-# LANGUAGE FlexibleInstances #-}

module Seri.Enoch.Prelude (
    apply, apply2, (<), (>),    
 ) where

import Prelude hiding ((<), (>))
import qualified Prelude

import Seri.Lambda
import Seri.Enoch.Enoch

instance SeriableT () where
    serit _ = unitT

instance SeriableE () where
    pack () = TExp unitE
    unpack (TExp e) | e == unitE = Just ()
    unpack _ = Nothing

instance SeriableT Integer where
    serit _ = integerT

instance SeriableE Integer where
    pack = TExp . integerE
    unpack (TExp (LitE (IntegerL i))) = Just i
    unpack _ = Nothing

instance Num (TExp Integer) where
    fromInteger = pack . fromInteger
    (+) = error $ "todo: (+) for TExp Integer"
    (*) = error $ "todo: (*) for TExp Integer"
    (-) = error $ "todo: (-) for TExp Integer"
    abs = error $ "todo: abs for TExp Integer"
    signum = error $ "todo: signum for TExp Integer"

instance SeriableT Bool where
    serit _ = boolT

instance SeriableE Bool where
    pack = TExp . boolE

    unpack (TExp x) | x Prelude.== trueE = Just True
    unpack (TExp x) | x Prelude.== falseE = Just False
    unpack _ = Nothing

instance SeriableT2 (->) where
    serit2 _ = ConT (name "->")

apply :: TExp (a -> b) -> TExp a -> TExp b
apply (TExp f) (TExp x) = TExp $ AppE f x

apply2 :: TExp (a -> b -> c) -> TExp a -> TExp b -> TExp c
apply2 f a b = apply (apply f a) b

(<) :: TExp Integer -> TExp Integer -> TExp Bool
(<) =
  let ltE :: TExp (Integer -> Integer -> Bool)
      ltE = varE "Seri.Lib.Prelude.<"
  in apply2 ltE

(>) :: TExp Integer -> TExp Integer -> TExp Bool
(>) =
  let gtE :: TExp (Integer -> Integer -> Bool)
      gtE = varE "Seri.Lib.Prelude.>"
  in apply2 gtE

