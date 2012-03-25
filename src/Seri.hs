
module Seri (
    Name, Type(..), Exp(..), Seriable(..)
    ) where

import qualified Language.Haskell.TH as TH

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
         | ThE (TH.Exp)
     deriving(Eq, Show)

class Seriable a where
    seriate :: a -> Exp

instance Seriable Exp where
    seriate = id

instance Seriable Integer where
    seriate = IntegerE
    
