
module Seri (
    Name, Type(..), Exp(..)
    ) where

type Name = String

data Type = IntegerT
          | ArrowT Type Type
          | UnknownT
      deriving(Eq, Show)

data Exp = IntegerE Integer
         | AddE Exp Exp
         | MulE Exp Exp
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | VarE Type Name
     deriving(Eq, Show)

