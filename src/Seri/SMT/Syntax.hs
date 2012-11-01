-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | An abstract syntax for SMT solvers.
module Seri.SMT.Syntax (
    Symbol, Command(..), Type(..), Expression(..),
    Binding, Literal(..),

    -- * Core
    letE, eqE, de_eqE, ifE, varE,
    boolE, trueE, falseE, notE, de_notE, andE, de_andE, orE, de_orE,

    -- * Integer
    integerE, ltE, leqE, gtE, addE, subE, mulE,

    -- * Bit Vector
    mkbvE, de_mkbvE, bvaddE, de_bvaddE, bvorE, de_bvorE, bvandE, de_bvandE,
    bvshiftLeft0E, de_bvshiftLeft0E,
    bvshiftRight0E, bvzeroExtendE, de_bvzeroExtendE, bvextractE, bvshlE,
  ) where

type Symbol = String

data Command = 
    Declare Symbol Type       -- ^ Declare a free variable of the given type
  | Assert Expression         -- ^ > (assert <expression>)
  | Check                     -- ^ > (check)
  | Push                      -- ^ > (push)
  | Pop                       -- ^ > (pop)
    deriving(Show, Eq)

data Type = 
    ArrowT [Type]           -- ^ > (-> <type> ... <type> <type>)
  | BitVectorT Integer      -- ^ > (bitvector <rational>)
  | IntegerT                -- ^ > int
  | BoolT                   -- ^ > bool
    deriving(Show, Eq)

data Expression =
    LitE Literal
  | VarE Symbol
  | LetE [Binding] Expression
  | AppE Expression [Expression]
  | UpdateE Expression [Expression] Expression
    deriving(Show, Eq)

type Binding = (String, Expression)

data Literal =
    BoolL Bool
  | IntegerL Integer
    deriving(Show, Eq)

-- | > A <symbol> expression.
varE :: String -> Expression
varE n = VarE n

-- | > true
trueE :: Expression
trueE = LitE (BoolL True)

-- | > false
falseE :: Expression
falseE = LitE (BoolL False)

boolE :: Bool -> Expression
boolE b = LitE (BoolL b)

-- | > not e
notE :: Expression -> Expression
notE e = AppE (varE "not") [e]

de_notE :: Expression -> Maybe Expression
de_notE (AppE (VarE "not") [e]) = Just e
de_notE _ = Nothing

-- | An integer expression.
integerE :: Integer -> Expression
integerE i = LitE (IntegerL i)

-- | > (= <expression> <expression>)
eqE :: Expression -> Expression -> Expression
eqE a b | a == trueE = b
eqE a b = AppE (varE "=") [a, b]

-- | Deconstruct an equality expression.
de_eqE :: Expression -> Maybe (Expression, Expression)
de_eqE (AppE (VarE "=") [a, b]) = Just (a, b)
de_eqE _ = Nothing

-- | > (and <term_1> ... <term_n>)
andE :: [Expression] -> Expression
andE es = 
  let flatten :: Expression -> [Expression]
      flatten e | e == trueE = []
      flatten (AppE f xs) | f == varE "and" = concat $ map flatten xs
      flatten e = [e]
  in case (concat $ map flatten es) of
      [] -> trueE
      [x] -> x
      xs | any (== falseE) xs -> falseE
      xs -> AppE (varE "and") xs

-- | Deconstruct an AND expression
de_andE :: Expression -> Maybe [Expression]
de_andE (AppE (VarE "and") args) = Just args
de_andE _ = Nothing

-- | > (or <term_1> ... <term_n>)
orE :: [Expression] -> Expression
orE es =
  let flatten :: Expression -> [Expression]
      flatten e | e == falseE = []
      flatten (AppE f xs) | f == varE "or" = concat $ map flatten xs
      flatten e = [e]
  in case (concat $ map flatten es) of
        [] -> falseE
        [x] -> x
        xs | any (== trueE) xs -> trueE
        xs -> AppE (varE "or") xs

-- | Deconstruct an OR expression
de_orE :: Expression -> Maybe [Expression]
de_orE (AppE (VarE "or") args) = Just args
de_orE _ = Nothing

-- | > (if <expression> <expression> <expression>)
ifE :: Expression -> Expression -> Expression -> Expression
ifE p a b | a == trueE = orE [p, b]
ifE p a b | a == falseE = andE [notE p, b]
ifE p a b | b == trueE = orE [notE p, a]
ifE p a b | b == falseE = andE [p, a]
ifE p a b | a == b = a
ifE p (AppE vif [p2, a, _]) b
    | vif == varE "if" && p == p2
    = ifE p a b
ifE p a b = AppE (varE "if") [p, a, b]

-- | > (< <exprsesion> <expression>)
ltE :: Expression -> Expression -> Expression
ltE a b = AppE (varE "<") [a, b]

-- | > (<= <exprsesion> <expression>)
leqE :: Expression -> Expression -> Expression
leqE a b = AppE (varE "<=") [a, b]

-- | > (> <exprsesion> <expression>)
gtE :: Expression -> Expression -> Expression
gtE a b = AppE (varE ">") [a, b]

-- | > (+ <exprsesion> <expression>)
addE :: Expression -> Expression -> Expression
addE a b = AppE (varE "+") [a, b]

-- | > (- <exprsesion> <expression>)
subE :: Expression -> Expression -> Expression
subE a b = AppE (varE "-") [a, b]

-- | > (* <exprsesion> <expression>)
mulE :: Expression -> Expression -> Expression
mulE a b = AppE (varE "*") [a, b]

-- | > (mk-bv w b)
mkbvE :: Integer -> Integer -> Expression
mkbvE w b = AppE (varE "mk-bv") [integerE w, integerE b]

de_mkbvE :: Expression -> Maybe (Integer, Integer)
de_mkbvE (AppE (VarE "mk-bv") [LitE (IntegerL w), LitE (IntegerL v)]) = Just (w, v)
de_mkbvE _ = Nothing

-- | > (bv-add a b)
bvaddE :: Expression -> Expression -> Expression
bvaddE a b = AppE (varE "bv-add") [a, b]

de_bvaddE :: Expression -> Maybe (Expression, Expression)
de_bvaddE (AppE (VarE "bv-add") [a, b]) = Just (a, b)
de_bvaddE _ = Nothing

-- | > (bv-or a b)
bvorE :: Expression -> Expression -> Expression
bvorE a b = AppE (varE "bv-or") [a, b]

de_bvorE :: Expression -> Maybe (Expression, Expression)
de_bvorE (AppE (VarE "bv-or") [a, b]) = Just (a, b)
de_bvorE _ = Nothing

-- | > (bv-and a b)
bvandE :: Expression -> Expression -> Expression
bvandE a b = AppE (varE "bv-and") [a, b]

de_bvandE :: Expression -> Maybe (Expression, Expression)
de_bvandE (AppE (VarE "bv-and") [a, b]) = Just (a, b)
de_bvandE _ = Nothing

-- | > (bv-shift-left0 a i)
bvshiftLeft0E :: Expression -> Integer -> Expression
bvshiftLeft0E a 0 = a
bvshiftLeft0E a b = AppE (varE "bv-shift-left0") [a, integerE b]

de_bvshiftLeft0E :: Expression -> Maybe (Expression, Integer)
de_bvshiftLeft0E (AppE (VarE "bv-shift-left0") [a, LitE (IntegerL b)]) = Just (a, b)
de_bvshiftLeft0E _ = Nothing

-- | > (bv-shift-right0 a i)
bvshiftRight0E :: Expression -> Integer -> Expression
bvshiftRight0E a 0 = a
bvshiftRight0E a b = AppE (varE "bv-shift-right0") [a, integerE b]

-- | > (bv-shl a b)
bvshlE :: Expression -> Expression -> Expression
bvshlE a b = AppE (varE "bv-shl") [a, b]

-- | > (bv-zero-extend a w)
bvzeroExtendE :: Expression -> Integer -> Expression
bvzeroExtendE a b = AppE (varE "bv-zero-extend") [a, integerE b]

de_bvzeroExtendE :: Expression -> Maybe (Expression, Integer)
de_bvzeroExtendE (AppE (VarE "bv-zero-extend") [a, LitE (IntegerL b)]) = Just (a, b)
de_bvzeroExtendE _ = Nothing

-- | > (bv-extract end begin bv)
bvextractE :: Integer -> Integer -> Expression -> Expression
bvextractE i j x = AppE (varE "bv-extract") [integerE i, integerE j, x]

letE :: [Binding] -> Expression -> Expression
letE [] e = e
letE [(n, e)] (VarE n') | n == n' = e
letE bs e = LetE bs e

