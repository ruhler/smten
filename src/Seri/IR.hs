
module Seri.IR (
    Name, Type(..), Exp(..),
    Seriable(..), Ppr(..),
    typeof,
    Traversal(..), TraversalM(..), traverse, traverseM,
    ) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH(Ppr(..))

type Name = String

data Type = IntegerT
          | BoolT
          | ArrowT Type Type
          | UnknownT
          | VarT Integer
      deriving(Eq, Show)

data Exp = BoolE Bool
         | IntegerE Integer
         | AddE Exp Exp
         | SubE Exp Exp
         | MulE Exp Exp
         | AppE Type Exp Exp
         | LamE Type Name Exp
         | VarE Type Name
         | ThE (TH.Exp)
     deriving(Eq, Show)

typeof :: Exp -> Type
typeof (BoolE _) = BoolT
typeof (IntegerE _) = IntegerT
typeof (AddE _ _) = IntegerT
typeof (MulE _ _) = IntegerT
typeof (AppE t _ _) = t
typeof (LamE t _ _) = t
typeof (VarE t _) = t

data TraversalM m a = TraversalM {
    tr_boolM :: Exp -> Bool -> m a,
    tr_intM :: Exp -> Integer -> m a,
    tr_addM :: Exp -> a -> a -> m a,
    tr_mulM :: Exp -> a -> a -> m a,
    tr_subM :: Exp -> a -> a -> m a,
    tr_appM :: Exp -> Type -> a -> a -> m a,
    tr_lamM :: Exp -> Type -> Name -> a -> m a,
    tr_varM :: Exp -> Type -> Name -> m a
}

data Traversal a = Traversal {
    tr_bool :: Exp -> Bool -> a,
    tr_int :: Exp -> Integer -> a,
    tr_add :: Exp -> a -> a -> a,
    tr_mul :: Exp -> a -> a -> a,
    tr_sub :: Exp -> a -> a -> a,
    tr_app :: Exp -> Type -> a -> a -> a,
    tr_lam :: Exp -> Type -> Name -> a -> a,
    tr_var :: Exp -> Type -> Name -> a
}

traverseM :: (Monad m) => TraversalM m a -> Exp -> m a
traverseM tr e@(BoolE b) = tr_boolM tr e b
traverseM tr e@(IntegerE i) = tr_intM tr e i
traverseM tr e@(AddE a b) = do
    a' <- traverseM tr a
    b' <- traverseM tr b
    tr_addM tr e a' b'
traverseM tr e@(MulE a b) = do
    a' <- traverseM tr a
    b' <- traverseM tr b
    tr_mulM tr e a' b'
traverseM tr e@(SubE a b) = do
    a' <- traverseM tr a
    b' <- traverseM tr b
    tr_subM tr e a' b'
traverseM tr e@(AppE t a b) = do
    a' <- traverseM tr a
    b' <- traverseM tr b
    tr_appM tr e t a' b'
traverseM tr e@(LamE t n b) = do
    b' <- traverseM tr b
    tr_lamM tr e t n b'
traverseM tr e@(VarE t n) = tr_varM tr e t n

traverse :: Traversal a -> Exp -> a
traverse tr e@(BoolE b) = tr_bool tr e b
traverse tr e@(IntegerE i) = tr_int tr e i
traverse tr e@(AddE a b) = tr_add tr e (traverse tr a) (traverse tr b)
traverse tr e@(MulE a b) = tr_mul tr e (traverse tr a) (traverse tr b)
traverse tr e@(SubE a b) = tr_sub tr e (traverse tr a) (traverse tr b)
traverse tr e@(AppE t a b) = tr_app tr e t (traverse tr a) (traverse tr b)
traverse tr e@(LamE t n b) = tr_lam tr e t n (traverse tr b)
traverse tr e@(VarE t n) = tr_var tr e t n

class Seriable a where
    seriate :: a -> Exp

instance Seriable Exp where
    seriate = id

instance Seriable Bool where 
    seriate = BoolE

instance Seriable Integer where
    seriate = IntegerE
    

instance Ppr Type where
    ppr BoolT = text "Bool"
    ppr IntegerT = text "Integer"
    ppr (ArrowT a b) = parens $ ppr a <+> text "->" <+> ppr b

pBoolE = 4
pIntegerE = 4
pAddE = 1
pSubE = 1
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

