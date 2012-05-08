
module Seri.Target.C.AST (
    Name, Type(..), Exp(..), Stmt(..), Dec(..),
    boolT, intT,
    ) where

import Seri.Ppr

type Name = String

data Type = BasicT String

data Exp = IntE Integer
         | AppE Name [Exp]

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

commalist :: [Doc] -> Doc
commalist [] = empty
commalist [x] = x
commalist (x:xs) = x <> comma <+> (commalist xs)

instance Ppr Type where
    ppr (BasicT str) = text str 

instance Ppr Exp where
    ppr (IntE i) = integer i
    ppr (AppE nm args) = text nm <> parens (commalist $ map ppr args)

instance Ppr Stmt where
    ppr (ReturnS e) = text "return" <+> ppr e <+> semi

instance Ppr Dec where
    ppr (FunD rt n args body)
      = let mkargs :: [(Type, Name)] -> Doc
            mkargs xs = commalist $ map (\(t, n) -> (ppr t) <+> text n) xs
        in ppr rt <+> text n <+> parens (mkargs args) <+> braces (ppr body)

