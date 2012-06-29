
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | An abstract syntax for yices2.
module Yices2.Syntax (
    Symbol, Command(..), Typedef(..), Type(..), Expression(..),
    VarDecl, Binding, ImmediateValue(..),
    varE,
    pretty,
  ) where

import Data.Ratio
import Text.PrettyPrint.HughesPJ

type Symbol = String

data Command = 
    DefineType Symbol (Maybe Typedef)
  | Define Symbol Type (Maybe Expression)
  | Assert Expression
  | Exit | Check | Push | Pop | Reset | ShowModel
  | Eval Expression
  | Echo String
  | Include String
  | SetParam Symbol ImmediateValue
  | ShowParam Symbol
  | ShowParams
  | ShowStats
  | ResetStats
  | SetTimeout Integer
  | DumpContext

data Typedef =
    ScalarTD [Symbol]
  | NormalTD Type
    

data Type = 
    VarT Symbol
  | TupleT [Type]
  | ArrowT [Type]
  | BitVectorT Integer
  | IntegerT
  | BoolT
  | RealT

data Expression =
    ImmediateE ImmediateValue
  | ForallE [VarDecl] Expression
  | ExistsE [VarDecl] Expression
  | LetE [Binding] Expression
  | UpdateE Expression [Expression] Expression
  | FunctionE Expression [Expression]

type VarDecl = (String, Type)
type Binding = (String, Expression)

data ImmediateValue =
    TrueV 
  | FalseV
  | VarV Symbol
  | RationalV Rational

varE :: String -> Expression
varE n = ImmediateE (VarV n)

-- | Convert an abstract syntactic construct to concrete yices2 syntax.
class Concrete a where
    concrete :: a -> Doc

instance Concrete Command where
    concrete (DefineType s Nothing)
        = parens $ text "define-type" <+> text s
    concrete (DefineType s (Just td))
        = parens $ text "define-type" <+> text s <+> concrete td
    concrete (Define s t Nothing)
        = parens $ text "define" <+> text s <+> text "::" <+> concrete t
    concrete (Define s t (Just e))
        = parens $ text "define" <+> text s <+> text "::" <+> concrete t <+> concrete e
    concrete (Assert e) = parens $ text "assert" <+> concrete e
    concrete Exit = parens $ text "exit"
    concrete Check = parens $ text "check"
    concrete Push = parens $ text "push"
    concrete Pop = parens $ text "pop"
    concrete Reset = parens $ text "reset"
    concrete ShowModel = parens $ text "show-model"
    concrete (Eval e) = parens $ text "eval-model" <+> concrete e
    concrete (Echo s) = parens $ text "echo" <+> concretestr s
    concrete (Include s) = parens $ text "include" <+> concretestr s
    concrete (SetParam s v)
        = parens $ text "set-param" <+> text s <+> concrete v
    concrete (ShowParam s) = parens $ text "show-param" <+> text s
    concrete ShowParams = parens $ text "show-params"
    concrete ShowStats = parens $ text "show-stats"
    concrete ResetStats = parens $ text "reset-stats"
    concrete (SetTimeout i) = parens $ text "set-timeout" <+> integer i
    concrete DumpContext = parens $ text "dump-context"

instance Concrete [Command] where
    concrete cmds = vcat (map concrete cmds)

instance Concrete Typedef where
    concrete (ScalarTD ss) = parens $ text "scalar" <+> hsep (map text ss)
    concrete (NormalTD t) = concrete t

instance Concrete Type where
    concrete (VarT s) = text s
    concrete (TupleT ts) = parens $ text "tuple" <+> hsep (map concrete ts)
    concrete (ArrowT ts) = parens $ text "->" <+> hsep (map concrete ts)
    concrete (BitVectorT i) = parens $ text "bitvector" <+> integer i
    concrete IntegerT = text "int"
    concrete BoolT = text "bool"
    concrete RealT = text "real"

instance Concrete Expression where
    concrete (ImmediateE iv) = concrete iv
    concrete (ForallE decls e)
        = parens $ text "forall" <+> parens (hsep $ map concrete decls) <+> concrete e
    concrete (ExistsE decls e)
        = parens $ text "exists" <+> parens (hsep $ map concrete decls) <+> concrete e
    concrete (LetE bindings e)
        = parens $ text "let" <+> parens (hsep $ map concrete bindings) <+> concrete e
    concrete (UpdateE f es e)
        = parens $ text "update" <+> concrete f
            <+> parens (hsep $ map concrete es) <+> concrete e
    concrete (FunctionE f args)
        = parens $ concrete f <+> hsep (map concrete args)

instance Concrete VarDecl where
    concrete (n, t) = text n <+> text "::" <+> concrete t

instance Concrete Binding where
    concrete (n, e) = text n <+> concrete e

instance Concrete ImmediateValue where
    concrete TrueV = text "true"
    concrete FalseV = text "false"
    concrete (VarV s) = text s
    concrete (RationalV r)
        = integer (numerator r) <+> text "/" <+> integer (denominator r)

concretestr :: String -> Doc
concretestr s = text (show s)

pretty :: Concrete a => a -> String
pretty x = render (concrete x)

