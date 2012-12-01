
module Seri.ExpH.Typeof (Typeof(..)) where

import Data.Maybe(fromMaybe)
import Data.Functor

import Seri.Type
import Seri.ExpH.ExpH


instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH s) = typeof s
    typeof (VarEH s) = typeof s
    typeof (PrimEH s _ _) = typeof s
    typeof (AppEH _ f x) = fromMaybe UnknownT $ snd <$> de_arrowT (typeof f)
    typeof (LamEH _ v f) = arrowsT [typeof v, typeof (f (VarEH v))]
    typeof (CaseEH _ _ _ _ e) = typeof e
    typeof (ErrorEH t _) = t


