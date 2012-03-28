
module Seri.IR (
    Name, Type(..), Exp(..),
    Ppr(..),
    typeof,
    ) where

import Language.Haskell.TH.PprLib
import Language.Haskell.TH(Ppr(..))

type Name = String

data Type = IntegerT
          | BoolT
          | ArrowT Type Type
      deriving(Eq, Show)

data Exp = BoolE Bool
         | IntegerE Integer
         | AddE Exp Exp
         | SubE Exp Exp
         | MulE Exp Exp
         | LtE Exp Exp
         | IfE Type Exp Exp Exp
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | FixE Type Name Exp
         | VarE Type Name
     deriving(Eq, Show)

typeof :: Exp -> Type
typeof (BoolE _) = BoolT
typeof (IntegerE _) = IntegerT
typeof (AddE _ _) = IntegerT
typeof (SubE _ _) = IntegerT
typeof (MulE _ _) = IntegerT
typeof (LtE _ _) = BoolT
typeof (IfE t _ _ _) = t
typeof (AppE t _ _) = t
typeof (LamE t _ _) = t
typeof (FixE t _ _) = t
typeof (VarE t _) = t

instance Ppr Type where
    ppr BoolT = text "Bool"
    ppr IntegerT = text "Integer"
    ppr (ArrowT a b) = parens $ ppr a <+> text "->" <+> ppr b

instance Ppr Exp where
    ppr (BoolE b) = if b then text "true" else text "false"
    ppr (IntegerE i) = integer i
    ppr (AddE a b) = parens $ ppr a <+> text "+" <+> ppr b
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

