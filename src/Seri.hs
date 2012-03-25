
module Seri (
    Name, Type(..), Exp(..), Seriable(..), typeof
    ) where

import qualified Language.Haskell.TH as TH

type Name = String

data Type = IntegerT
          | ArrowT Type Type
          | UnknownT
          | VarT Integer
      deriving(Eq, Show)

data Exp = IntegerE Integer
         | AddE Exp Exp
         | MulE Exp Exp
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | VarE Type Name
         | ThE (TH.Exp)
     deriving(Eq, Show)

typeof :: Exp -> Type
typeof (IntegerE _) = IntegerT
typeof (AddE _ _) = IntegerT
typeof (MulE _ _) = IntegerT
typeof (AppE t _ _) = t
typeof (LamE t _ _) = t
typeof (VarE t _) = t

class Seriable a where
    seriate :: a -> Exp

instance Seriable Exp where
    seriate = id

instance Seriable Integer where
    seriate = IntegerE
    
