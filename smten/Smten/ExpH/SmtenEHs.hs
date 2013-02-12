
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.SmtenEHs (stringEH) where

import Data.Maybe
import Data.Typeable

import Smten.Type
import Smten.Lit
import Smten.Sig
import Smten.Name
import Smten.ExpH.ExpH
import Smten.ExpH.SmtenEH
import Smten.ExpH.Sugar
import Smten.ExpH.TH

derive_SmtenEH ''Maybe
derive_SmtenEH ''[]
derive_SmtenEH ''(,)

stringEH :: String -> ExpH
stringEH = smtenEH

