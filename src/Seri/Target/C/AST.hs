
module Seri.Target.C.AST (
    Name, Type(..), Exp(..), Stmt(..), Dec(..),
    boolT, intT,
    ) where

import Seri.Utils.Ppr

type Name = String

data Type = BasicT String
          | StructT String
    deriving(Eq, Show)

data Exp = IntE Integer
         | AppE Name [Exp]
         | CondE Exp Exp Exp
    deriving(Eq, Show)

data Stmt = ReturnS Exp
    deriving(Eq, Show)

data Dec = FunD Type Name [(Type, Name)] Stmt
    deriving(Eq, Show)

commalist :: [Doc] -> Doc
commalist [] = empty
commalist [x] = x
commalist (x:xs) = x <> comma <+> (commalist xs)

boolT :: Type
boolT = intT

intT :: Type
intT = BasicT "int"

instance Ppr Type where
    ppr (BasicT str) = text str 
    ppr (StructT str) = text "struct" <+> text str 

instance Ppr Exp where
    ppr (IntE i) = integer i
    ppr (AppE nm args) = text nm <> parens (commalist $ map ppr args)
    ppr (CondE p a b) = parens (ppr p <+> text "?" <+> ppr a <+> text ":" <+> ppr b)

instance Ppr Stmt where
    ppr (ReturnS e) = text "return" <+> ppr e <+> semi

instance Ppr Dec where
    ppr (FunD rt n args body)
      = let mkargs :: [(Type, Name)] -> Doc
            mkargs xs = commalist $ map (\(t, n) -> (ppr t) <+> text n) xs
        in ppr rt <+> text n <+> parens (mkargs args) <+> braces (ppr body)

