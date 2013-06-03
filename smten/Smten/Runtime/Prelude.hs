
module Smten.Runtime.Prelude where

import qualified Prelude
import Prelude hiding (Bool(..))

data Bool = False | True

__caseTrue :: Bool -> z -> z -> z
__caseTrue x y n = 
   case x of
      True -> y
      _ -> n

__caseFalse :: Bool -> z -> z -> z
__caseFalse x y n =
   case x of
     False -> y
     _ -> n

