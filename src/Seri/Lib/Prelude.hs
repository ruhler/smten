
module Seri.Lib.Prelude (
    preludeR,
    module Seri.Lib.Bool,
    module Seri.Lib.Char,
    module Seri.Lib.Integer,
    module Seri.Lib.List,
    module Seri.Lib.Maybe,
    module Seri.Lib.Monad,
    module Seri.Lib.Tuple,
    module Seri.Lib.Unit,
    ) where

import Seri
import Seri.Lib.Bool
import Seri.Lib.Char
import Seri.Lib.Integer
import Seri.Lib.List
import Seri.Lib.Maybe
import Seri.Lib.Monad
import Seri.Lib.Tuple
import Seri.Lib.Unit

preludeR :: (Monad m) => Rule m
preludeR = rules [coreR, integerR]

