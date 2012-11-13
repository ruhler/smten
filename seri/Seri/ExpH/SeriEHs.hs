
{-# LANGUAGE TemplateHaskell #-}

module Seri.ExpH.SeriEHs () where

import Seri.Type
import Seri.ExpH.SeriEH
import Seri.ExpH.TH

derive_SeriEH ''Maybe
derive_SeriEH ''[]
derive_SeriEH ''(,)

