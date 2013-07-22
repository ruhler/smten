
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Base (
    module Smten.Smten.Base
 )  where

import Smten.Runtime.SmtenHS
import Smten.Smten.Base

instance SmtenHS0 Char where
    error0 = error "TODO: Char.error0"
    realize0 = error "TODO: Char.realize0"
    ite0 = error "TODO: Char.ite0"

instance SmtenHS0 Int where
    error0 = error "TODO: Int.error0"
    realize0 = error "TODO: Int.realize0"
    ite0 = error "TODO: Int.ite0"

instance SmtenHS0 Integer where
    error0 = error "TODO: Integer.error0"
    realize0 = error "TODO: Integer.realize0"
    ite0 = error "TODO: Integer.ite0"

instance SmtenHS0 () where
    error0 = error "TODO: Unit.error0"
    realize0 = error "TODO: Unit.realize0"
    ite0 = error "TODO: Unit.ite0"

instance SmtenHS1 [] where
    error1 = error "TODO: List.error1"
    realize1 = error "TODO: List.realize1"
    ite1 = error "TODO: List.ite1"

