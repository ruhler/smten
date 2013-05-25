
{-# LANGUAGE PatternGuards #-}

-- | More syntactic sugar for ExpH.
-- These make use of SmtenEHs.
module Smten.ExpH.Sugar2 (
    de_stringEH, de_listEH,
    ) where

import Control.Monad

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar
import Smten.ExpH.SmtenEH
import Smten.ExpH.SmtenEHs

de_stringEH :: ExpH -> Maybe String
de_stringEH = de_smtenEH

de_listEH :: ExpH -> Maybe [ExpH]
de_listEH = de_smtenEH

