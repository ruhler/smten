
{-# LANGUAGE TemplateHaskell #-}

module Seri.ExpH.SeriEHs (stringEH) where

import Data.Maybe
import Data.Typeable

import Seri.Type
import Seri.Lit
import Seri.Sig
import Seri.Name
import Seri.ExpH.ExpH
import Seri.ExpH.SeriEH
import Seri.ExpH.Sugar
import Seri.ExpH.TH

derive_SeriEH ''Maybe
derive_SeriEH ''[]
derive_SeriEH ''(,)

stringEH :: String -> ExpH
stringEH = seriEH

