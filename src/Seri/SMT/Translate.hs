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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

-- Translate a Seri expression to SMT.
module Seri.SMT.Translate (
    Compilation(), compilation, CompilationM, runCompilation,
    smtN, smtT, smtE, smtD,
    ) where

import Debug.Trace

import qualified Seri.SMT.Syntax as SMT

import qualified Data.Map as Map

import Data.List ((\\))
import Data.Char(ord)
import Data.Maybe(catMaybes)
import Data.Functor

import Control.Monad.State.Strict
import Control.Monad.Error

import Seri.Failable
import Seri.Lambda
import Seri.Strict
import Seri.Elaborate

-- | An SMT compilation object.
data Compilation = Compilation {
    ys_nocaseerr :: Bool,       -- ^ Assume an alternative will match in each case expression
    ys_poly :: Env,             -- ^ The polymorphic seri environment

    -- | Declarations needed for what was compiled, stored in reverse order
    -- for efficiency sake.
    ys_cmdsr :: [SMT.Command],

    ys_errid :: Integer,        -- ^ unique id to use for next free error variable
    ys_caseid :: Integer        -- ^ unique id to use for next case arg variable
}

-- | Monad for performing additional smt compilation.
type CompilationM = StateT Compilation Failable

-- | Append a list of commands in order to the commands specified so far.
addcmds :: [SMT.Command] -> CompilationM ()
addcmds cmds = modifyS $ \ys -> ys { ys_cmdsr = (reverse cmds) ++ ys_cmdsr ys}

-- | Create a new smt compilation object.
compilation :: Bool         -- ^ nocase err?
               -> Env          -- ^ polymorphic seri environment
               -> Compilation
compilation nocaseerr poly = Compilation {
    ys_nocaseerr = nocaseerr,
    ys_poly = poly,
    ys_cmdsr = [],
    ys_errid = 1,
    ys_caseid = 1 
}

-- | Run a compilation.
runCompilation :: CompilationM a -> Compilation -> Failable (a, Compilation)
runCompilation = runStateT

-- Given the argument type and output type of a free error variable, return
-- the smt name of a newly defined one.
yfreeerr :: Type -> CompilationM String
yfreeerr t = do
    yt <- smtT t
    id <- gets ys_errid
    let nm = "err~" ++ show id
    modifyS $ \ys -> ys { ys_errid = id+1 }
    addcmds [SMT.Define nm yt Nothing]
    return nm

-- Get a new, free variable for use as a case argument variable.
yfreecase :: Char -> CompilationM String
yfreecase c = do
    id <- gets ys_caseid
    modifyS $ \ys -> ys { ys_caseid = id+1 }
    return $! ['c', c, '~'] ++ show id

-- Generate smt code for a fully applied constructor application.
smtC :: Sig -> [Exp] -> CompilationM SMT.Expression
smtC (Sig n _) [] | n == name "True" = return SMT.trueE
smtC (Sig n _) [] | n == name "False" = return SMT.falseE
smtC s _ = error $ "smtC: " ++ pretty s

-- | Convert a seri name to an SMT name.
smtN :: Name -> String
smtN = unname

-- | Compile a seri type to a smt type
-- Before using the returned type, the smtD function should be called to
-- get the required smt declarations.
smtT :: Type -> CompilationM SMT.Type
smtT t | t == boolT = return SMT.BoolT
smtT t | t == integerT = return SMT.IntegerT
smtT t | t == charT = return SMT.IntegerT
smtT t | Just w <- deBitT t = return $ SMT.BitVectorT w
smtT t | Just (a, b) <- deArrowT t = SMT.ArrowT <$> mapM smtT [a, b] 

-- | Compile a seri expression to a smt expression.
-- Before using the returned expression, the smtD function should be called
-- to get the required smt declarations.
smtE :: Exp -> CompilationM SMT.Expression
smtE e = do
  poly <- gets ys_poly
  let se = elaborate SNF poly e
  smtE' se

-- | Compile a seri expression to smt, assuming the expression can be
-- represented as is in smt without further elaboration.
smtE' :: Exp -> CompilationM SMT.Expression
smtE' e | Just (VarP (Sig n _), v, x) <- deLet1E e = do
    v' <- smtE' v
    x' <- smtE' x
    return (SMT.letE [(smtN n, v')] x')

smtE' e | Just (p, a, b) <- deIfE e = do
  p' <- smtE' p
  a' <- smtE' a
  b' <- smtE' b
  return (SMT.ifE p' a' b') 

smtE' e | Just (xs, ms) <- deCaseE e = do
  nocaseerr <- gets ys_nocaseerr
  (let -- depat p e
     --    outputs: (predicate, bindings)
     --   predicate - predicates indicating if the 
     --                pattern p matches expression e
     --   bindings - a list of bindings made when p matches e.
     depat :: Pat -> SMT.Expression -> CompilationM ([SMT.Expression], [SMT.Binding])
     depat (ConP _ n []) e | n == name "True" = return ([e], [])
     depat (ConP _ n []) e | n == name "False" = return ([SMT.notE e], [])
     depat (VarP (Sig n t)) e = return ([], [(pretty n, e)])
     depat (LitP (IntegerL i)) e = return ([SMT.eqE (SMT.integerE i) e], [])
     depat (LitP (CharL c)) e = return ([SMT.eqE (SMT.integerE (fromIntegral $ ord c)) e], [])
     depat (WildP _) _ = return ([], [])
     depat p x = error $ "depat: " ++ pretty p

     -- dematch es ms
     --    es - the expressions being cased on
     --    ms - the remaining matches in the case statement.
     --  outputs - the smt expression implementing the matches.
     dematch :: [SMT.Expression] -> [Match] -> CompilationM SMT.Expression
     dematch ye [] = do
         errnm <- yfreeerr (arrowsT $ map typeof xs ++ [typeof (head ms)])
         return $ SMT.FunctionE (SMT.varE errnm) ye
     dematch es [Match ps b] | nocaseerr = do
         b' <- smtE' b
         bindings <- concatMap snd <$> mapM (uncurry depat) (zip ps es)
         return $ SMT.letE bindings b'
     dematch es ((Match ps b):ms) = do
         bms <- dematch es ms
         b' <- smtE' b
         (preds, bindings) <- unzip <$> mapM (uncurry depat) (zip ps es)
         let pred = SMT.andE (concat preds)
         let lete = SMT.letE (concat bindings) b'
         return $ SMT.ifE pred lete bms

     givename :: (SMT.Expression, Char) -> CompilationM ([SMT.Binding], SMT.Expression)
     givename (e@SMT.ImmediateE {}, _) = return ([], e)
     givename (e, c) = do
        cnm <- yfreecase c
        return ([(cnm, e)], SMT.varE cnm)
   in do
       -- The expressions e' can get really big, so we don't want to duplicate
       -- them when we use them it to check for a pattern match in every
       -- alternative. Instead we bind them to variables ~ca, ~cb, ... and
       -- duplicate that instead.
       es' <- mapM smtE' xs
       (binds, es'') <- unzip <$> mapM givename (zip es' "abcdefghijklmnopqrstuvwxyz")
       body <- dematch es'' ms
       return $ SMT.letE (concat binds) body
   )
smtE' (AppE a []) = smtE' a
smtE' e@(AppE a b) =
    case unappsE e of 
       ((ConE s):args) -> smtC s args
       [VarE (Sig n t), _]
            | n == name "Seri.Lib.Prelude.error"
            , Just (_, dt) <- deArrowT t
            -> do errnm <- yfreeerr dt
                  return $ SMT.varE errnm
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.<"
          -> binary SMT.ltE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.<="
          -> binary SMT.leqE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.>"
          -> binary SMT.gtE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.&&"
          -> binary (\x y -> SMT.andE [x, y]) a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.||"
          -> binary (\x y -> SMT.orE [x, y]) a b
       [VarE (Sig n _), a]
          | n == name "Seri.Lib.Prelude.not"
          -> SMT.notE <$> smtE' a
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.__prim_add_Integer"
          -> binary SMT.addE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.__prim_sub_Integer"
          -> binary SMT.subE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.__prim_mul_Integer"
          -> binary SMT.mulE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Prelude.__prim_eq_Integer"
          -> binary SMT.eqE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Bit.__prim_eq_Bit"
          -> binary SMT.eqE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Bit.__prim_add_Bit"
          -> binary SMT.bvaddE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Bit.__prim_or_Bit"
          -> binary SMT.bvorE a b
       [VarE (Sig n _), a, b]
          | n == name "Seri.Lib.Bit.__prim_and_Bit"
          -> binary SMT.bvandE a b
       -- TODO: should we allow shifting by an amount not statically
       -- determined? In that case, I think we need to convert the second
       -- argument to a bit vector in order to use smt bvshl function.
       [VarE (Sig n _), a, (LitE (IntegerL v))]
          | n == name "Seri.Lib.Bit.__prim_lsh_Bit"
          -> do
           a' <- smtE' a
           return (SMT.bvshiftLeft0E a' v)
       [VarE (Sig n _), a, (LitE (IntegerL v))] | n == name "Seri.Lib.Bit.__prim_rshl_Bit" -> do
           a' <- smtE' a
           return (SMT.bvshiftRight0E a' v)
       [VarE (Sig n t), LitE (IntegerL x)]
            | n == name "Seri.Lib.Bit.__prim_fromInteger_Bit"
            , Just (_, bt) <- deArrowT t
            , Just w <- deBitT bt
            -> return (SMT.mkbvE w x)
       [VarE (Sig n t), a]
            | n == name "Seri.Lib.Bit.__prim_zeroExtend_Bit"
            , Just (bs, bt) <- deArrowT t
            , Just sw <- deBitT bs
            , Just tw <- deBitT bt
            -> do
               a' <- smtE' a
               return (SMT.bvzeroExtendE a' (tw - sw))
       [VarE (Sig n t), a]
            | n == name "Seri.Lib.Bit.__prim_truncate_Bit"
            , Just (_, bt) <- deArrowT t
            , Just tw <- deBitT bt
            -> SMT.bvextractE (tw - 1) 0 <$> smtE' a
       [VarE (Sig n _), x, LitE (IntegerL i)]
            | n == name "Seri.Lib.Bit.__prim_extract_Bit"
            , Just tw <- deBitT (typeof e)
            -> SMT.bvextractE (i + tw - 1) i <$> smtE' x
       [VarE (Sig n _), f, k, v] | n == name "Seri.SMT.Array.update" -> do
           f' <- smtE' f
           k' <- smtE' k
           v' <- smtE' v
           return $ SMT.UpdateE f' [k'] v'
       _ -> do
           a' <- smtE' (AppE a (init b))
           b' <- smtE' (last b)
           return $ SMT.FunctionE a' [b']
smtE' (LitE (IntegerL x)) = return $ SMT.integerE x
smtE' (LitE (CharL c)) = return $ SMT.integerE (fromIntegral $ ord c)
smtE' l@(LaceE ms) = 
    throw $ "lambda expression in smt target generation: " ++ pretty l
smtE' (ConE s) = smtC s []
smtE' (VarE (Sig n _)) = return $ SMT.varE (smtN n)


-- | Take the list of smt declarations made so far.
-- The returned list does not include any of the previously taken declarations
-- by calling smtD.
smtD :: CompilationM [SMT.Command]
smtD = do
  cmds <- gets $ reverse . ys_cmdsr
  modifyS $ \ys -> ys { ys_cmdsr = [] }
  return cmds

binary :: (SMT.Expression -> SMT.Expression -> SMT.Expression)
           -> Exp -> Exp -> CompilationM SMT.Expression
binary f a b = do
    a' <- smtE' a
    b' <- smtE' b
    return $! (f a' b')

