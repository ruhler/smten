
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Print yices syntax prettily
module Yices.Pretty (pretty) where

import Data.Ratio

import Yices.Syntax

-- | Convert an abstract syntactic construct to concrete yices syntax.
class Concrete a where
    concrete :: YicesVersion -> a -> [String]

indent :: [String] -> [String]
indent = map ((:) ' ')

instance Concrete Command where
    concrete v (DefineType s Nothing)
        = ["(define-type " ++ s ++ ")"]
    concrete v (DefineType s (Just td))
        = ["(define-type " ++ s]
          ++ indent (concrete v td)
          ++ [")"]
    concrete v (Define s t Nothing)
        = ["(define " ++ s ++ " ::"]
          ++ indent (concrete v t)
          ++ [")"]
    concrete v (Define s t (Just e))
        = ["(define " ++ s ++ " ::"]
          ++ indent (concrete v t)
          ++ indent (concrete v e)
          ++ [")"]
    concrete v (Assert e)
        = ["(assert"]
          ++ indent (concrete v e)
          ++ [")"]
    concrete v Check = ["(check)"]
    concrete v Push = ["(push)"]
    concrete v Pop = ["(pop)"]

concrete' :: (Concrete a) => YicesVersion -> a -> [String]
concrete' v x =
  let lines = concrete v x
  in if (sum (map length lines) < 72)
        then [concat lines]
        else lines

instance Concrete [Command] where
    concrete v cmds = concat $ map (concrete' v) cmds

instance Concrete Typedef where
    concrete v (ScalarTD ss) = ["(scalar " ++ concat (map ((:) ' ') ss) ++ ")"]
    concrete v (NormalTD t) = concrete' v t

instance Concrete Type where
    concrete v (VarT s) = [s]
    concrete v (TupleT ts)
      = ["(tuple"]
        ++ indent (concat $ map (concrete' v) ts)
        ++ [")"]
    concrete v (ArrowT ts)
      = ["(->"]
        ++ indent (concat $ map (concrete' v) ts)
        ++ [")"]
    concrete v (BitVectorT i) = ["(bitvector " ++ show i ++ ")"]
    concrete v IntegerT = ["int"]
    concrete v BoolT = ["bool"]
    concrete v RealT = ["real"]

instance Concrete Expression where
    concrete v (ImmediateE iv) = concrete' v iv
    concrete v (ForallE decls e)
        = ["(forall"]
          ++ indent (["("] ++ indent (concat $ map (concrete' v) decls) ++ [")"])
          ++ indent (concrete' v e)
          ++ [")"]
    concrete v (ExistsE decls e)
        = ["(exists"]
          ++ indent (["("] ++ indent (concat $ map (concrete' v) decls) ++ [")"])
          ++ indent (concrete' v e)
          ++ [")"]
    concrete v (LetE bindings e)
        = ["(let"]
          ++ indent (["("] ++ indent (concat $ map (concrete' v) bindings) ++ [")"])
          ++ indent (concrete' v e)
          ++ [")"]
    concrete v (UpdateE f es e)
        = ["(update"]
          ++ indent (concrete' v f)
          ++ indent (["("] ++ indent (concat $ map (concrete' v) es) ++ [")"])
          ++ indent (concrete' v e)
          ++ [")"]
    concrete v (TupleUpdateE t i x)
        = [(if v == Yices1 then "(update" else "(tuple-update")]
          ++ indent (concrete' v t)
          ++ indent [show i]
          ++ indent (concrete' v x)
          ++ [")"]
    concrete v (FunctionE f args)
        = ["("]
          ++ indent (concrete' v f)
          ++ indent (concat $ map (concrete' v) args)
          ++ [")"]

instance Concrete VarDecl where
    concrete v (n, t) = [n ++ " ::"] ++ indent (concrete' v t)

instance Concrete Binding where
    concrete v (n, e) = ["(" ++ n] ++ indent (concrete' v e) ++ [")"]

instance Concrete ImmediateValue where
    concrete v TrueV = ["true"]
    concrete v FalseV = ["false"]
    concrete v (VarV s) = [s]
    concrete v (RationalV r)
        = [show (numerator r) ++
            if denominator r == 1
                then ""
                else "/" ++ show (denominator r)]

-- | Render abstract yices syntax to a concrete syntax string.
pretty :: Concrete a => YicesVersion -> a -> String
pretty v x = unlines (concrete' v x)

