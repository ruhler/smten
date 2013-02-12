
{-# LANGUAGE TemplateHaskell #-}

module Smten.Type.SmtenTs () where

import Smten.Type.SmtenT
import Smten.Type.TH

derive_SmtenT ''Maybe
derive_SmtenT ''[]

