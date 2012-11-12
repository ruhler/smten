
{-# LANGUAGE TemplateHaskell #-}

module Seri.Type.SeriTs () where

import Seri.Type.SeriT
import Seri.Type.TH

derive_SeriT ''Maybe
derive_SeriT ''[]

