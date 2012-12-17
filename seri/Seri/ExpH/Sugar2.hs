
{-# LANGUAGE PatternGuards #-}

-- | More syntactic sugar for ExpH.
-- These make use of SeriEHs.
module Seri.ExpH.Sugar2 (
    de_stringEH, de_listEH,
    ) where

import Control.Monad

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEH
import Seri.ExpH.SeriEHs
import Seri.ExpH.Typeof

de_stringEH :: ExpH -> Maybe String
de_stringEH = de_seriEH

de_listEH :: ExpH -> Maybe [ExpH]
de_listEH = de_seriEH

