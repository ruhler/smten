
module Seri.Target.C.AST (
    Name, Type(..), Exp(..), Stmt(..), Dec(..),
    boolT, intT,
    ) where

import Seri.Ppr

type Name = String

data Type = BasicT String

data Exp = IntE Integer

data Stmt = ReturnS Exp

data Dec = FunD Type Name [(Type, Name)] Stmt

boolT :: Type
boolT = intT

trueE :: Exp
trueE = intE 1

falseE :: Exp
falseE = intE 0

intE :: Integer -> Exp
intE = IntE

intT :: Type
intT = BasicT "int"

instance Ppr Type where
    ppr (BasicT str) = text str 

instance Ppr Exp where
    ppr (IntE i) = integer i

instance Ppr Stmt where
    ppr (ReturnS e) = text "return" <+> ppr e <+> semi

instance Ppr Dec where
    ppr (FunD rt n args body)
      = let mkargs :: [(Type, Name)] -> Doc
            mkargs [] = empty
            mkargs [(t, n)] = (ppr t) <+> text n
            mkargs ((t, n):xs) = (ppr t) <+> text n <> comma <+> (mkargs xs)
        in ppr rt <+> text n <+> parens (mkargs args) <+> braces (ppr body)

