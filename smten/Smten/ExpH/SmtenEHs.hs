
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.SmtenEHs (stringEH) where

import Smten.ExpH.ExpH
import Smten.ExpH.SmtenEH
import Smten.ExpH.Sugar
import Smten.ExpH.TH

derive_SmtenEH "Prelude" ''Maybe
derive_SmtenEH "Prelude" ''Either
derive_SmtenEH "Prelude" ''[]
derive_SmtenEH "Prelude" ''(,)

stringEH :: String -> ExpH
stringEH = smtenEH

