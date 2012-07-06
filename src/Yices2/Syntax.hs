
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | An abstract syntax for yices2.
module Yices2.Syntax (
    Symbol, Command(..), Typedef(..), Type(..), Expression(..),
    VarDecl, Binding, ImmediateValue(..),
    trueE, varE, integerE, selectE, eqE, andE, ifE, ltE, gtE,
    addE, subE,
    tupleE, tupleUpdateE,
    pretty,
  ) where

import Data.Ratio
import Text.PrettyPrint.HughesPJ

type Symbol = String

data Command = 
    DefineType Symbol (Maybe Typedef)           -- ^ > (define-type <symbol> [<typedef>])
  | Define Symbol Type (Maybe Expression)       -- ^ > (define <symbol> :: <type> [<expression>])
  | Assert Expression                           -- ^ > (assert <expression>)
  | Exit                                        -- ^ > (exit)
  | Check                                       -- ^ > (check)
  | Push                                        -- ^ > (push)
  | Pop                                         -- ^ > (pop)
  | Reset                                       -- ^ > (reset)
  | ShowModel                                   -- ^ > (show-model)
  | Eval Expression                             -- ^ > (eval <expression>)
  | Echo String                                 -- ^ > (echo <string>)
  | Include String                              -- ^ > (include <string>)
  | SetParam Symbol ImmediateValue              -- ^ > (set-param <symbol> <immediate-value>)
  | ShowParam Symbol                            -- ^ > (show-param <symbol>)
  | ShowParams                                  -- ^ > (show-params)
  | ShowStats                                   -- ^ > (show-stats)
  | ResetStats                                  -- ^ > (reset-stats)
  | SetTimeout Integer                          -- ^ > (set-timeout <number>)
  | DumpContext                                 -- ^ > (dump-context)

data Typedef =
    NormalTD Type           -- ^ > <type>
  | ScalarTD [Symbol]       -- ^ > (scalar <symbol> ... <symbol>)
    

data Type = 
    VarT Symbol             -- ^ > <symbol>
  | TupleT [Type]           -- ^ > (tuple <type> ... <type>)
  | ArrowT [Type]           -- ^ > (-> <type> ... <type> <type>)
  | BitVectorT Integer      -- ^ > (bitvector <rational>)
  | IntegerT                -- ^ > int
  | BoolT                   -- ^ > bool
  | RealT                   -- ^ > real

data Expression =
    ImmediateE ImmediateValue           -- ^ > <immediate-value>
  | ForallE [VarDecl] Expression        -- ^ > (forall (<var_decl> ... <var_decl>) <expression>)
  | ExistsE [VarDecl] Expression        -- ^ > (exists (<var_decl> ... <var_decl>) <expression>)
  | LetE [Binding] Expression           -- ^ > (let (<binding> ... <binding>) <expression>)
  | UpdateE Expression [Expression] Expression  -- ^ > (update <expression> (<expression> ... <expression>) <expression>)
  | FunctionE Expression [Expression]   -- ^ > (<function> <expression> ... <expression>)

type VarDecl = (String, Type)           -- ^ > <symbol> :: <type>
type Binding = (String, Expression)     -- ^ > (<symbol> <expression>)

data ImmediateValue =
    TrueV               -- ^ > true
  | FalseV              -- ^ > false
  | RationalV Rational  -- ^ > <rational>
  | VarV Symbol         -- ^ > symbol

-- | > true
trueE :: Expression
trueE = ImmediateE TrueV

-- | > A <symbol> expression.
varE :: String -> Expression
varE n = ImmediateE (VarV n)

-- | An integer expression.
integerE :: Integer -> Expression
integerE i = ImmediateE (RationalV (fromInteger i))

-- | > (select <tuple> i)
selectE :: Expression -> Integer -> Expression
selectE e i = FunctionE (varE "select") [e, integerE i]

-- | > (= <expression> <expression>)
eqE :: Expression -> Expression -> Expression
eqE a b = FunctionE (varE "=") [a, b]

-- | > (mk-tuple <term_1>  ... <term_n>)
tupleE :: [Expression] -> Expression
tupleE [] = error "tupleE: empty list"
tupleE args = FunctionE (varE "mk-tuple") args

-- | > (tuple-update <tuple> i <term>)
tupleUpdateE :: Expression -> Integer -> Expression -> Expression
tupleUpdateE tpl idx nv
    = FunctionE (varE "tuple-update") [tpl, integerE idx, nv]

-- | > (and <term_1> ... <term_n>)
andE :: [Expression] -> Expression
andE [] = trueE
andE [x] = x
andE xs = FunctionE (varE "and") xs

-- | > (if <expression> <expression> <expression>)
ifE :: Expression -> Expression -> Expression -> Expression
ifE p a b = FunctionE (varE "if") [p, a, b]

-- | > (< <exprsesion> <expression>)
ltE :: Expression -> Expression -> Expression
ltE a b = FunctionE (varE "<") [a, b]

-- | > (> <exprsesion> <expression>)
gtE :: Expression -> Expression -> Expression
gtE a b = FunctionE (varE ">") [a, b]

-- | > (+ <exprsesion> <expression>)
addE :: Expression -> Expression -> Expression
addE a b = FunctionE (varE "+") [a, b]

-- | > (- <exprsesion> <expression>)
subE :: Expression -> Expression -> Expression
subE a b = FunctionE (varE "-") [a, b]

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
        = parens $ text "forall" <+> parens (sep $ map concrete decls) <+> concrete e
    concrete (ExistsE decls e)
        = parens $ text "exists" <+> parens (sep $ map concrete decls) <+> concrete e
    concrete (LetE bindings e)
        = parens $ sep [text "let", parens (sep $ map concrete bindings), concrete e]
    concrete (UpdateE f es e)
        = parens $ text "update" <+> concrete f
            <+> parens (hsep $ map concrete es) <+> concrete e
    concrete (FunctionE f args)
        = parens $ sep ((concrete f) : (map concrete args))

instance Concrete VarDecl where
    concrete (n, t) = text n <+> text "::" <+> concrete t

instance Concrete Binding where
    concrete (n, e) = parens $ text n <+> concrete e

instance Concrete ImmediateValue where
    concrete TrueV = text "true"
    concrete FalseV = text "false"
    concrete (VarV s) = text s
    concrete (RationalV r)
        = integer (numerator r) <>
            if denominator r == 1
                then empty
                else text "/" <> integer (denominator r)

concretestr :: String -> Doc
concretestr s = text (show s)

-- | Render abstract yices syntax to a concrete syntax string.
pretty :: Concrete a => a -> String
pretty x = render (concrete x)

