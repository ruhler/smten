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

{-# LANGUAGE PatternGuards #-}

-- | An abstract syntax for SMT solvers.
module Smten.SMT.Syntax (
    Symbol, Command(..), Type(..), Expression(..),
    Binding, Literal(..),

    -- * Core
    letE, de_letE, eqE, de_eqE, ifE, de_ifE, varE, de_varE,
    boolE, de_boolE, trueE, de_trueE, falseE, de_falseE,
    notE, de_notE, andE, de_andE, orE, de_orE,

    -- * Integer
    integerE, de_integerE,
    ltE, de_ltE, leqE, de_leqE, gtE, de_gtE, geqE, de_geqE,
    addE, de_addE, subE, de_subE, mulE, de_mulE,

    -- * Bit Vector
    mkbvE, de_mkbvE, bvaddE, de_bvaddE, bvsubE, de_bvsubE,
    bvltE, bvgtE, bvleqE, bvgeqE,
    de_bvltE, de_bvgtE, de_bvleqE, de_bvgeqE,
    bvorE, de_bvorE, bvandE, de_bvandE,
    bvnotE, de_bvnotE,
    bvshlE, de_bvshlE, bvlshrE, de_bvlshrE,
    bvzeroExtendE, de_bvzeroExtendE, bvextractE,
    bvconcatE, de_bvconcatE,
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

de_varE :: Expression -> Maybe String
de_varE (VarE n) = Just n
de_varE _ = Nothing

-- | > true
trueE :: Expression
trueE = LitE (BoolL True)

-- | > false
falseE :: Expression
falseE = LitE (BoolL False)

boolE :: Bool -> Expression
boolE b = LitE (BoolL b)

de_boolE :: Expression -> Maybe Bool
de_boolE (LitE (BoolL b)) = Just b
de_boolE _ = Nothing

de_trueE :: Expression -> Bool
de_trueE (LitE (BoolL b)) = b
de_trueE _ = False

de_falseE :: Expression -> Bool
de_falseE (LitE (BoolL b)) = not b
de_falseE _ = False

-- | > not e
notE :: Expression -> Expression
notE p
 | Just v <- de_boolE p = boolE (not v)
 | otherwise = AppE (varE "not") [p]

de_notE :: Expression -> Maybe Expression
de_notE (AppE (VarE "not") [e]) = Just e
de_notE _ = Nothing

-- | An integer expression.
integerE :: Integer -> Expression
integerE i = LitE (IntegerL i)

de_integerE :: Expression -> Maybe Integer
de_integerE (LitE (IntegerL i)) = Just i
de_integerE _ = Nothing

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
andE es
 | any de_falseE es = falseE
 | otherwise =
     case filter (not . de_trueE) es of
        [] -> trueE
        [x] -> x
        xs -> AppE (varE "and") xs

-- | Deconstruct an AND expression
de_andE :: Expression -> Maybe [Expression]
de_andE (AppE (VarE "and") args) = Just args
de_andE _ = Nothing

-- | > (or <term_1> ... <term_n>)
orE :: [Expression] -> Expression
orE es
 | any de_trueE es = trueE
 | otherwise =
     case filter (not . de_falseE) es of
        [] -> falseE
        [x] -> x
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

de_ifE :: Expression -> Maybe (Expression, Expression, Expression)
de_ifE (AppE (VarE "if") [p, a, b]) = Just (p, a, b)
de_ifE _ = Nothing

-- | > (< <exprsesion> <expression>)
ltE :: Expression -> Expression -> Expression
ltE a b = AppE (varE "<") [a, b]

de_ltE :: Expression -> Maybe (Expression, Expression)
de_ltE (AppE (VarE "<") [a, b]) = Just (a, b)
de_ltE _ = Nothing

-- | > (<= <exprsesion> <expression>)
leqE :: Expression -> Expression -> Expression
leqE a b = AppE (varE "<=") [a, b]

de_leqE :: Expression -> Maybe (Expression, Expression)
de_leqE (AppE (VarE "<=") [a, b]) = Just (a, b)
de_leqE _ = Nothing

-- | > (> <exprsesion> <expression>)
gtE :: Expression -> Expression -> Expression
gtE a b = AppE (varE ">") [a, b]

de_gtE :: Expression -> Maybe (Expression, Expression)
de_gtE (AppE (VarE ">") [a, b]) = Just (a, b)
de_gtE _ = Nothing

-- | > (> <exprsesion> <expression>)
geqE :: Expression -> Expression -> Expression
geqE a b = AppE (varE ">=") [a, b]

de_geqE :: Expression -> Maybe (Expression, Expression)
de_geqE (AppE (VarE ">=") [a, b]) = Just (a, b)
de_geqE _ = Nothing

-- | > (+ <exprsesion> <expression>)
addE :: Expression -> Expression -> Expression
addE a b = AppE (varE "+") [a, b]

de_addE :: Expression -> Maybe (Expression, Expression)
de_addE (AppE (VarE "+") [a, b]) = Just (a, b)
de_addE _ = Nothing

-- | > (- <exprsesion> <expression>)
subE :: Expression -> Expression -> Expression
subE a b = AppE (varE "-") [a, b]

de_subE :: Expression -> Maybe (Expression, Expression)
de_subE (AppE (VarE "-") [a, b]) = Just (a, b)
de_subE _ = Nothing

-- | > (* <exprsesion> <expression>)
mulE :: Expression -> Expression -> Expression
mulE a b = AppE (varE "*") [a, b]

de_mulE :: Expression -> Maybe (Expression, Expression)
de_mulE (AppE (VarE "*") [a, b]) = Just (a, b)
de_mulE _ = Nothing

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

bvsubE :: Expression -> Expression -> Expression
bvsubE a b = AppE (varE "bv-sub") [a, b]

de_bvsubE :: Expression -> Maybe (Expression, Expression)
de_bvsubE (AppE (VarE "bv-sub") [a, b]) = Just (a, b)
de_bvsubE _ = Nothing

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

bvnotE :: Expression -> Expression
bvnotE a = AppE (varE "bv-not") [a]

de_bvnotE :: Expression -> Maybe Expression
de_bvnotE (AppE (VarE "bv-not") [a]) = Just a
de_bvnotE _ = Nothing

-- | > (bv-shl a b)
bvshlE :: Expression -> Expression -> Expression
bvshlE a b = AppE (varE "bv-shl") [a, b]

de_bvshlE :: Expression -> Maybe (Expression, Expression)
de_bvshlE (AppE (VarE "bv-shl") [a, b]) = Just (a, b)
de_bvshlE _ = Nothing

bvlshrE :: Expression -> Expression -> Expression
bvlshrE a b = AppE (varE "bv-rshl") [a, b]

de_bvlshrE :: Expression -> Maybe (Expression, Expression)
de_bvlshrE (AppE (VarE "bv-lshr") [a, b]) = Just (a, b)
de_bvlshrE _ = Nothing

-- | > (bv-zero-extend a w)
bvzeroExtendE :: Expression -> Integer -> Expression
bvzeroExtendE a b = AppE (varE "bv-zero-extend") [a, integerE b]

de_bvzeroExtendE :: Expression -> Maybe (Expression, Integer)
de_bvzeroExtendE (AppE (VarE "bv-zero-extend") [a, LitE (IntegerL b)]) = Just (a, b)
de_bvzeroExtendE _ = Nothing

-- | > (bv-extract end begin bv)
bvextractE :: Integer -> Integer -> Expression -> Expression
bvextractE i j x = AppE (varE "bv-extract") [integerE i, integerE j, x]

bvconcatE :: Expression -> Expression -> Expression
bvconcatE a b = AppE (varE "bv-concat") [a, b]

de_bvconcatE :: Expression -> Maybe (Expression, Expression)
de_bvconcatE (AppE (VarE "bv-concat") [a, b]) = Just (a, b)
de_bvconcatE _ = Nothing

letE :: [Binding] -> Expression -> Expression
letE [] e = e
letE [(n, e)] (VarE n') | n == n' = e
letE bs e = LetE bs e

de_letE :: Expression -> Maybe ([Binding], Expression)
de_letE (LetE bs e) = Just (bs, e)
de_letE _ = Nothing

bvltE :: Expression -> Expression -> Expression
bvltE a b = AppE (varE "bv-lt") [a, b]

de_bvltE :: Expression -> Maybe (Expression, Expression)
de_bvltE (AppE (VarE "bv-lt") [a, b]) = Just (a, b)
de_bvltE _ = Nothing

bvgtE :: Expression -> Expression -> Expression
bvgtE a b = AppE (varE "bv-gt") [a, b]

de_bvgtE :: Expression -> Maybe (Expression, Expression)
de_bvgtE (AppE (VarE "bv-gt") [a, b]) = Just (a, b)
de_bvgtE _ = Nothing

bvgeqE :: Expression -> Expression -> Expression
bvgeqE a b = AppE (varE "bv-ge") [a, b]

de_bvgeqE :: Expression -> Maybe (Expression, Expression)
de_bvgeqE (AppE (VarE "bv-ge") [a, b]) = Just (a, b)
de_bvgeqE _ = Nothing

bvleqE :: Expression -> Expression -> Expression
bvleqE a b = AppE (varE "bv-le") [a, b]

de_bvleqE :: Expression -> Maybe (Expression, Expression)
de_bvleqE (AppE (VarE "bv-le") [a, b]) = Just (a, b)
de_bvleqE _ = Nothing

