
{-# LANGUAGE TemplateHaskell #-}

module Smten.Type.SmtenTs () where

import Smten.Type.SmtenT
import Smten.Type.TH

derive_SmtenT "Prelude" ''Maybe
derive_SmtenT "Prelude" ''Either
derive_SmtenT "Prelude" ''[]

