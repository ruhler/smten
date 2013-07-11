
module Smten.Runtime.Formula (
    TypeF(..), BoolF(..),
    andF, notF,
    ) where

import Smten.Runtime.FreeID

data TypeF = BoolTF

data BoolF =
      TrueF
    | FalseF
    | IteF BoolF BoolF BoolF
    | AndF BoolF BoolF
    | NotF BoolF
    | VarF FreeID

andF :: BoolF -> BoolF -> BoolF
andF TrueF x = x
andF FalseF x = FalseF
andF a b = AndF a b

notF :: BoolF -> BoolF
notF TrueF = FalseF
notF FalseF = TrueF
notF (NotF x) = x
notF x = NotF x

