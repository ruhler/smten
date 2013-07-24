
module Smten.Runtime.Formula (
    TypeF(..), AnyF(..), BoolF(..),
    andF, notF, iteF,
    ) where

import Smten.Runtime.FreeID

data TypeF = BoolTF
    deriving (Show)

data AnyF = BoolF BoolF

data BoolF =
      TrueF
    | FalseF
    | IteF BoolF BoolF BoolF
    | AndF BoolF BoolF
    | NotF BoolF
    | VarF FreeID
    deriving (Show)

andF :: BoolF -> BoolF -> BoolF
andF TrueF x = x
andF FalseF x = FalseF
andF a TrueF = a
andF a FalseF = FalseF
andF a b = AndF a b

notF :: BoolF -> BoolF
notF TrueF = FalseF
notF FalseF = TrueF
notF (NotF x) = x
notF x = NotF x

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF TrueF x _ = x
iteF FalseF _ x = x
iteF (NotF x) a b = iteF x b a
iteF p TrueF FalseF = p
iteF p FalseF TrueF = notF p
iteF p a b = IteF p a b

