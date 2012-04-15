
module Seri.Lib.Prelude (
    preludeB, preludeR,
    module Seri.Lib.Bool,
    module Seri.Lib.Integer,
    module Seri.Lib.List,
    module Seri.Lib.Maybe,
    module Seri.Lib.Tuple,
    module Seri.Lib.Unit,
    ) where

import Seri
import Seri.Lib.Bool
import Seri.Lib.Integer
import Seri.Lib.List
import Seri.Lib.Maybe
import Seri.Lib.Tuple
import Seri.Lib.Unit

preludeB :: Builtin
preludeB = builtins [integerB]

preludeR :: Rule
preludeR = rules [coreR, integerR]

