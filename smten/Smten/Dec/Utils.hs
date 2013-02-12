
module Smten.Dec.Utils () where

import Smten.Type
import Smten.Dec.Dec

instance Assign Class where
    assignl f (Class nm ts) = Class nm (assignl f ts)

