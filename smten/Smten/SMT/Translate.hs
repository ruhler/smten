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

-- Translate a Smten expression to SMT.
module Smten.SMT.Translate (
    Compilation(), compilation, CompilationM, runCompilation,
    smtN, smtT, smtE, smtD,
    ) where

import Debug.Trace

import qualified Smten.SMT.Syntax as SMT

import qualified Data.Map as Map

import Data.List ((\\))
import Data.Char(ord)
import Data.Maybe(catMaybes)
import Data.Functor

import Control.Monad.State.Strict
import Control.Monad.Error

import Smten.Failable
import Smten.Name
import Smten.Bit
import Smten.Lit
import Smten.Sig
import Smten.Ppr
import Smten.Type
import Smten.Exp
import Smten.Dec
import Smten.Strict

-- | An SMT compilation object.
data Compilation = Compilation {
    -- | Declarations needed for what was compiled, stored in reverse order
    -- for efficiency sake.
    ys_cmdsr :: [SMT.Command],

    ys_errid :: Integer        -- ^ unique id to use for next free error variable
}

-- | Monad for performing additional smt compilation.
type CompilationM = StateT Compilation Failable

-- | Append a list of commands in order to the commands specified so far.
addcmds :: [SMT.Command] -> CompilationM ()
addcmds cmds = modifyS $ \ys -> ys { ys_cmdsr = (reverse cmds) ++ ys_cmdsr ys}

-- | Create a new smt compilation object.
compilation :: Compilation
compilation = Compilation {
    ys_cmdsr = [],
    ys_errid = 1
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
    addcmds [SMT.Declare nm yt]
    return nm

-- Generate smt code for a fully applied constructor application.
smtC :: Sig -> [Exp] -> CompilationM SMT.Expression
smtC (Sig n _) [] | n == name "True" = return SMT.trueE
smtC (Sig n _) [] | n == name "False" = return SMT.falseE
smtC s args = throw $ "unsupported constructor application: " ++ show (s, args)

-- | Convert a smten name to an SMT name.
smtN :: Name -> String
smtN = unname

-- | Compile a smten type to a smt type
-- Before using the returned type, the smtD function should be called to
-- get the required smt declarations.
smtT :: Type -> CompilationM SMT.Type
smtT t | t == boolT = return SMT.BoolT
smtT t | t == integerT = return SMT.IntegerT
smtT t | t == charT = return SMT.IntegerT
smtT t | Just w <- de_bitT t = return $ SMT.BitVectorT w
smtT t = throw $ "smtT: unsupported type: " ++ pretty t

-- | Compile a smten expression to a smt expression.
-- Before using the returned expression, the smtD function should be called
-- to get the required smt declarations.
smtE :: Exp -> CompilationM SMT.Expression
smtE e = smtE' e `catchError` (\msg -> throw $ msg ++ "\nWhen translating: " ++ pretty e)

-- | Compile a smten expression to smt, assuming the expression can be
-- represented as is in smt without further elaboration.
smtE' :: Exp -> CompilationM SMT.Expression
smtE' e | Just (Sig n _, v, x) <- de_letE e = do
    v' <- smtE' v
    x' <- smtE' x
    return (SMT.letE [(smtN n, v')] x')

smtE' e
 | Just v <- de_integerE e = return $ SMT.integerE v
 | Just v <- de_charE e = return $ SMT.integerE (fromIntegral $ ord v)
 | Just v <- de_bitE e = return $ SMT.mkbvE (bv_width v) (bv_value v)
smtE' (ConE s) = smtC s []
smtE' (VarE (Sig n _)) = return $ SMT.varE (smtN n)
smtE' e@(AppE a b) =
    case de_appsE e of 
       (ConE s, args) -> smtC s args
       (VarE (Sig n t), [_])
            | n == name "Prelude.error"
            , Just (_, dt) <- de_arrowT t
            -> do errnm <- yfreeerr dt
                  return $ SMT.varE errnm
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_lt_Integer" -> binary SMT.ltE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_leq_Integer" -> binary SMT.leqE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_gt_Integer" -> binary SMT.gtE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_geq_Integer" -> binary SMT.geqE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_add_Integer" -> binary SMT.addE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_sub_Integer" -> binary SMT.subE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_mul_Integer" -> binary SMT.mulE a b
       (VarE (Sig n _), [a, b]) | n == name "Prelude.__prim_eq_Integer" -> binary SMT.eqE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_eq_Bit" -> binary SMT.eqE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_lt_Bit" -> binary SMT.bvltE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_leq_Bit" -> binary SMT.bvleqE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_gt_Bit" -> binary SMT.bvgtE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_geq_Bit" -> binary SMT.bvgeqE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_add_Bit" -> binary SMT.bvaddE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_sub_Bit" -> binary SMT.bvsubE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_or_Bit" -> binary SMT.bvorE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_and_Bit" -> binary SMT.bvandE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_concat_Bit" -> binary SMT.bvconcatE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_shl_Bit" -> binary SMT.bvshlE a b
       (VarE (Sig n _), [a, b]) | n == name "Smten.Bit.__prim_lshr_Bit" -> binary SMT.bvlshrE a b
       (VarE (Sig n _), [a]) | n == name "Smten.Bit.__prim_not_Bit" -> SMT.bvnotE <$> smtE' a
       (VarE (Sig n t), [a])
            | n == name "Smten.Bit.__prim_zeroExtend_Bit"
            , Just (bs, bt) <- de_arrowT t
            , Just sw <- de_bitT bs
            , Just tw <- de_bitT bt
            -> do
               a' <- smtE' a
               return (SMT.bvzeroExtendE a' (tw - sw))
       (VarE (Sig n t), [a])
            | n == name "Smten.Bit.__prim_signExtend_Bit"
            , Just (bs, bt) <- de_arrowT t
            , Just sw <- de_bitT bs
            , Just tw <- de_bitT bt
            -> do
               a' <- smtE' a
               return (SMT.bvsignExtendE a' (tw - sw))
       (VarE (Sig n t), [a])
            | n == name "Smten.Bit.__prim_truncate_Bit"
            , Just (_, bt) <- de_arrowT t
            , Just tw <- de_bitT bt
            -> SMT.bvextractE (tw - 1) 0 <$> smtE' a
       (VarE (Sig n _), [x, li])
            | n == name "Smten.Bit.__prim_extract_Bit"
            , Just i <- de_integerE li
            , Just tw <- de_bitT (typeof e)
            -> SMT.bvextractE (i + tw - 1) i <$> smtE' x
       _ -> do
           a' <- smtE' a
           b' <- smtE' b
           return $ SMT.AppE a' [b']
smtE' l@(LamE {}) = throw $ "lambda expression in smt target generation: " ++ show l
smtE' (CaseE x (Sig nm _) y n) | nm == name "True" = do
    x' <- smtE' x
    y' <- smtE' y
    n' <- smtE' n
    return $ SMT.ifE x' y' n'
smtE' (CaseE x (Sig nm _) y n) | nm == name "False" = do
    x' <- smtE' x
    y' <- smtE' y
    n' <- smtE' n
    return $ SMT.ifE x' n' y'
smtE' e@(CaseE {})
  = throw $ "unsupported case expression in smt target generation: " ++ show e


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

