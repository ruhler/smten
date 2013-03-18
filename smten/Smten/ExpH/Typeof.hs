
module Smten.ExpH.Typeof (Typeof(..)) where

import Data.Maybe(fromMaybe)
import Data.Functor

import Smten.Type
import Smten.Sig
import Smten.ExpH.ExpH


instance Typeof ExpH where
    typeof (LitEH l) = typeof l
    typeof (ConEH _ _ t _) = t
    typeof (VarEH s) = typeof s
    typeof (PrimEH _ _ t _ _) = t
    typeof (LamEH _ (Sig _ it) ot _) = arrowT it ot
    typeof (IfEH _ t _ _ _) = t


