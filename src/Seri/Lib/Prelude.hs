
module Seri.Lib.Prelude (
    preludeR,
    ) where

import Seri.Lambda
import Seri.Lib.Integer

preludeR :: (Monad m) => Rule m
preludeR = rules [coreR, integerR]

