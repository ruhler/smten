
{-# LANGUAGE FlexibleInstances #-}

module Seri.Enoch.Prelude (
    apply, apply2, (<), (>),    
 ) where

import Prelude hiding ((<), (>))
import qualified Prelude

import Seri.Lambda
import Seri.Enoch.Enoch

instance Seriable () where
    pack () = TExp unitE
    unpack (TExp e) | e == unitE = Just ()
    unpack _ = Nothing
    serit _ = unitT

instance Seriable Integer where
    pack = TExp . integerE
    unpack (TExp (LitE (IntegerL i))) = Just i
    unpack _ = Nothing
    serit _ = integerT

instance Num (TExp Integer) where
    fromInteger = pack . fromInteger
    (+) = error $ "todo: (+) for TExp Integer"
    (*) = error $ "todo: (*) for TExp Integer"
    (-) = error $ "todo: (-) for TExp Integer"
    abs = error $ "todo: abs for TExp Integer"
    signum = error $ "todo: signum for TExp Integer"

instance Seriable Bool where
    pack = TExp . boolE

    unpack (TExp x) | x Prelude.== trueE = Just True
    unpack (TExp x) | x Prelude.== falseE = Just False
    unpack _ = Nothing

    serit _ = boolT


apply :: TExp (a -> b) -> TExp a -> TExp b
apply (TExp f) (TExp x) = TExp $ AppE f x

apply2 :: TExp (a -> b -> c) -> TExp a -> TExp b -> TExp c
apply2 f a b = apply (apply f a) b

(<) :: TExp Integer -> TExp Integer -> TExp Bool
(<) = apply2 (TExp $ VarE (Sig (name "Seri.Lib.Prelude.<") (arrowsT [integerT, integerT, boolT])))

(>) :: TExp Integer -> TExp Integer -> TExp Bool
(>) = apply2 (TExp $ VarE (Sig (name "Seri.Lib.Prelude.>") (arrowsT [integerT, integerT, boolT])))

