
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.Ppr () where

import Smten.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.FromExpH

instance Ppr ExpH where
    ppr e = ppr (fromExpH e)

