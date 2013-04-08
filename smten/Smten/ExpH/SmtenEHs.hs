
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

derive_SmtenEH "Prelude" ''Maybe
derive_SmtenEH "Prelude" ''[]
derive_SmtenEH "Prelude" ''(,)

stringEH :: String -> ExpH
stringEH = smtenEH

