
module Seri.HaskellF.Lib.Prelude (
    Symbolic__(..), Bool(),
    __mkTrue, __mkFalse, __caseTrue, __caseFalse,
    ) where

import qualified Prelude

data Bool = True
          | False
          | Free Prelude.String
          | Conditional Bool Bool Bool

__mkTrue :: Bool
__mkTrue = True

__mkFalse :: Bool
__mkFalse = False

__caseTrue :: (Symbolic__ a) => Bool -> a -> a -> a
__caseTrue True y _ = y
__caseTrue False _ n = n
__caseTrue p y n = __if p y n

__caseFalse :: (Symbolic__ a) => Bool -> a -> a -> a
__caseFalse True _ n = n
__caseFalse False y _ = y
__caseFalse p y n = __if p n y

class Symbolic__ a where
    __if :: Bool -> a -> a -> a

instance Symbolic__ Bool where
    __if = Conditional 

