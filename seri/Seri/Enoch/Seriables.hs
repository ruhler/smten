
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Enoch.Seriables () where

import Seri.Lambda
import Seri.Type.SeriT
import Seri.Enoch.Enoch
import Seri.Enoch.EnochTH

derive_SeriT ''Maybe
derive_SeriableE ''Maybe

derive_SeriT ''[]
derive_SeriableE ''[]

