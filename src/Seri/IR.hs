
module Seri.IR (
    Name, Type(..), Exp(..),
    Seriable(..), Ppr(..),
    typeof
    ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH(Ppr(..))

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
    

instance Ppr Type where
    ppr IntegerT = text "Integer"
    ppr (ArrowT a b) = parens $ ppr a <+> text "->" <+> ppr b

pIntegerE = 4
pAddE = 1
pMulE = 2
pAppE = 3
pLamE = 0
pVarE = 4

precedence :: Exp -> Integer
precedence (IntegerE _) = pIntegerE
precedence (AddE _ _) = pAddE
precedence (MulE _ _) = pMulE
precedence (AppE _ _ _) = pAppE
precedence (LamE _ _ _) = pLamE
precedence (VarE _ _) = pVarE

prec :: Integer -> Exp -> Doc
prec i e
 = if (i > precedence e) 
     then parens $ ppr e
     else ppr e

instance Ppr Exp where
    ppr (IntegerE i) = integer i
    ppr (AddE a b) = prec pAddE a <+> text "+" <+> prec pAddE b
    ppr (MulE a b) = prec pMulE a <+> text "*" <+> prec pMulE b
    ppr (AppE _ a b) = prec pAppE a <+> prec pAppE b
    ppr (LamE _ n b) = text "\\" <> text n <+> text "->" <+> prec pLamE b
    ppr (VarE _ n) = text n

