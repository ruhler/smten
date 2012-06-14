
module Seri.Target.Elaborate(
    elaborateR,
    module Seri.Target.Elaborate.Elaborate,
    module Seri.Target.Elaborate.Integer
    ) where

import Seri.Target.Elaborate.Elaborate
import Seri.Target.Elaborate.Integer

elaborateR :: (Monad m) => Rule m
elaborateR = rules [integerR, coreR]

