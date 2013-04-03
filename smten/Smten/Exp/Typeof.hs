
module Smten.Exp.Typeof () where

import Data.Functor
import Data.Maybe(fromMaybe)

import Smten.Type
import Smten.Exp.Exp

instance Typeof Exp where
    typeof (LitE _ l) = typeof l
    typeof (ConE _ s) = typeof s
    typeof (VarE _ s) = typeof s
    typeof (AppE _ f x) = fromMaybe UnknownT $ snd <$> de_arrowT (typeof f)
    typeof (LamE _ s b) = arrowT (typeof s) (typeof b)
    typeof (CaseE _ _ _ _ n) = typeof n

