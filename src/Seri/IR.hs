
module Seri.IR (
    Name, Type(..), Primitive(..), Exp(..),
    ) where

import Seri.Ppr

type Name = String

data Type = IntegerT
          | BoolT
          | ArrowT Type Type
      deriving(Eq, Show)

data Primitive = AddP
      deriving(Eq, Show)

data Exp = BoolE Bool
         | IntegerE Integer
         | PrimE Type Primitive
         | SubE Exp Exp
         | MulE Exp Exp
         | LtE Exp Exp
         | IfE Type Exp Exp Exp
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | FixE Type Name Exp
         | VarE Type Name
     deriving(Eq, Show)

instance Ppr Type where
    ppr BoolT = text "Bool"
    ppr IntegerT = text "Integer"
    ppr (ArrowT a b) = parens $ ppr a <+> text "->" <+> ppr b

instance Ppr Primitive where
    ppr AddP = text "+"

instance Ppr Exp where
    ppr (BoolE b) = if b then text "true" else text "false"
    ppr (IntegerE i) = integer i
    ppr (PrimE _ p) = ppr p
    ppr (SubE a b) = parens $ ppr a <+> text "-" <+> ppr b
    ppr (MulE a b) = parens $ ppr a <+> text "*" <+> ppr b
    ppr (LtE a b) = parens $ ppr a <+> text "<" <+> ppr b
    ppr (IfE _ p a b) = parens $ text "if" <+> ppr p
                        <+> text "then" <+> ppr a
                        <+> text "else" <+> ppr b
    ppr (AppE _ a b) = parens $ ppr a <+> ppr b
    ppr (LamE _ n b) = parens $ text "\\" <> text n <+> text "->" <+> ppr b
    ppr (FixE _ n b) = parens $ text "!" <> text n <+> ppr b 
    ppr (VarE _ n) = text n

