
module Seri.ExpH.Typeof (Typeof(..)) where

import Data.Maybe(fromMaybe)
import Data.Functor

import Seri.Type
import Seri.Sig
import Seri.ExpH.ExpH


instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH _ t _) = t
    typeof (VarEH s) = typeof s
    typeof (PrimEH _ t _ _) = t
    typeof (AppEH f x) = fromMaybe UnknownT $ snd <$> de_arrowT (typeof f)
    typeof (LamEH (Sig _ it) ot _) = arrowT it ot
    typeof (CaseEH _ _ _ e) = typeof e
    typeof (ErrorEH t _) = t


