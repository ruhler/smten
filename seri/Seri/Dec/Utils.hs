
module Seri.Dec.Utils () where

import Seri.Type
import Seri.Dec.Dec

instance Assign Class where
    assignl f (Class nm ts) = Class nm (assignl f ts)

