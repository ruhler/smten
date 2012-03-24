
module Seri (
    Name, Type(..), Exp(..)
    ) where

type Name = String

data Type = IntegerT
          | ArrowT Type Type
      deriving(Eq, Show)

data Exp = IntegerE Integer
         | AddE Exp Exp
         | MulE Exp Exp
         | AppE Type Exp Exp
         | LamE Type Type Name Exp  -- argtype bodytype arg body
         | VarE Type Name
     deriving(Eq, Show)


