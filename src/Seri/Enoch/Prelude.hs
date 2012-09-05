
module Seri.Enoch.Prelude (
 ) where

import Prelude hiding ((<), (>))
import qualified Prelude

import Seri.Lambda
import Seri.Enoch.Enoch

instance Seriable Integer where
    pack = TExp . integerE
    unpack (TExp (LitE (IntegerL i))) = Just i
    unpack _ = Nothing
    serit _ = integerT

instance Seriable Bool where
    pack = TExp . boolE
    unpack (TExp x) | x Prelude.== trueE = Just True
    unpack (TExp x) | x Prelude.== falseE = Just False
    unpack _ = Nothing

apply :: TExp (a -> b) -> TExp a -> TExp b
apply f x = TExp $ AppE f x

apply2 :: TExp (a -> b -> c) -> TExp a -> TExp b -> TExp c
apply2 f a b = apply (apply f a) b

(<) :: TExp Integer -> TExp Integer -> TExp Bool
(<) = apply2 (TExp $ SigE "Seri.Lib.Prelude.<" (arrowsT [integerT, integerT, boolT]))

(>) :: TExp Integer -> TExp Integer -> TExp Bool
(>) = apply2 (TExp $ SigE "Seri.Lib.Prelude.>" (arrowsT [integerT, integerT, boolT]))

