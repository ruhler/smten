
module C (
    Name, Type(..), Exp(..), Stmt(..), Dec(..)
    ) where

type Name = String

data Type = IntT
      deriving (Eq, Show)

data Exp = IntE Integer
         | AddE Exp Exp
         | MulE Exp Exp
         | AppE Exp [Exp]
         | VarE Name
     deriving (Eq, Show)

data Stmt = ReturnS Exp

data Dec = FunD Name [(Type, Name)] Type [Stmt]


