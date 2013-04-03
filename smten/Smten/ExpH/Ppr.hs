
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.Ppr () where

import Smten.Lit
import Smten.Sig
import Smten.Type
import Smten.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.FromExpH
import Smten.ExpH.Sugar
import Smten.ExpH.Sugar2

instance Ppr ExpH where
    ppr e = ppr (fromExpH e)

