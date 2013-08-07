
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Smten.Smten.TypeLits (
    Nat, SingI, type (+),
 ) where

import GHC.TypeLits
import Smten.Smten.TypeLitsTH

-- Tell GHC about addition. At least for numbers up to 256.
-- If you need math for bigger numbers, expand them here.
mksuccs [0..256]
mkadds [2..256]
type instance a + b = TAdd a b

