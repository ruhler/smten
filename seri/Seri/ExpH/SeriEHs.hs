
{-# LANGUAGE TemplateHaskell #-}

module Seri.ExpH.SeriEHs (stringEH) where

import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.SeriEH
import Seri.ExpH.TH

derive_SeriEH ''Maybe
derive_SeriEH ''[]
derive_SeriEH ''(,)

stringEH :: String -> ExpH
stringEH = seriEH

