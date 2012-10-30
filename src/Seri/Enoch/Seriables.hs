
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Enoch.Seriables () where

import Seri.Lambda
import Seri.Enoch.Enoch
import Seri.Enoch.EnochTH

derive_SeriableT ''Maybe
derive_SeriableE ''Maybe

derive_SeriableT ''[]
derive_SeriableE ''[]

