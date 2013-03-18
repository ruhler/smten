
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
import Smten.ExpH.Typeof

de_stringEH :: Thunk -> Maybe String
de_stringEH = de_smtenEH

de_listEH :: Thunk -> Maybe [Thunk]
de_listEH = de_smtenEH

