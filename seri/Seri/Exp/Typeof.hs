
module Seri.Exp.Typeof () where

import Data.Functor
import Data.Maybe(fromMaybe)

import Seri.Type.Type
import Seri.Type.Sugar
import Seri.Type.Typeof
import Seri.Exp.Exp

instance Typeof Exp where
    typeof (LitE l) = typeof l
    typeof (ConE s) = typeof s
    typeof (VarE s) = typeof s
    typeof (AppE f x) = fromMaybe UnknownT $ snd <$> de_arrowT (typeof f)
    typeof (LamE s b) = arrowT (typeof s) (typeof b)
    typeof (CaseE _ _ _ n) = typeof n

