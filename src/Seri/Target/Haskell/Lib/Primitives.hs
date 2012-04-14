
module Seri.Target.Haskell.Lib.Primitives (
    (*), (+),
 ) where

import Prelude (Integer)
import qualified Prelude

(*) :: Integer -> Integer -> Integer
(*) = (Prelude.*)

(+) :: Integer -> Integer -> Integer
(+) = (Prelude.+)

