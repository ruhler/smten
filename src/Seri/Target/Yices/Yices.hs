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

module Seri.Target.Yices.Yices (
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
import Seri.Target.Elaborate

-- | An SMT compilation object.
data Compilation = Compilation {
    ys_nocaseerr :: Bool,       -- ^ Assume an alternative will match in each case expression
    ys_poly :: Env,             -- ^ The polymorphic seri environment

    -- | map of already compiled types
    ys_types :: Map.Map Type SMT.Type,

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
    ys_types = Map.empty,
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
yCon :: Sig -> [Exp] -> CompilationM SMT.Expression
yCon (Sig n _) [] | n == name "True" = return SMT.trueE
yCon (Sig n _) [] | n == name "False" = return SMT.falseE
yCon (Sig n ct) args = do
    let dt = last $ unarrowsT ct
    smtT dt   -- make sure uidt~dt is defined.
    let tagged = SMT.tupleUpdateE (SMT.varE $ smtuidt (mononametype dt)) smtti (SMT.varE $ smtN n)
    if null args
        then return tagged
        else do
            args' <- mapM smtE' args
            ci <- smtci n
            return $ SMT.tupleUpdateE tagged ci (SMT.tupleE args')

-- Given the name of a data type, return an uninterpreted constant of that
-- type.
smtuidt :: Name -> String
smtuidt n = "uidt~" ++ unname n

-- Given the name of a data type, return the name of it's tag type.
smttag :: Name -> String
smttag n = "tag~" ++ unname n

-- Given the name of a constructor, return the index for its fields in the
-- data types tuple.
smtci :: Name -> CompilationM Integer
smtci n =
    let findidx :: (MonadError String m) => Integer -> [Con] -> m Integer
        findidx _ [] = throw $ "index for " ++ pretty n ++ " not found"
        findidx i ((Con cn []) : cs) = findidx i cs
        findidx i ((Con cn _) : _) | n == cn = return i
        findidx i (_ : cs) = findidx (i+1) cs
    in do
        env <- gets ys_poly
        contype <- lookupDataConType env n
        case head . unappsT . last . unarrowsT $ contype of
            ConT dt -> do
                (DataD _ _ cs) <- lookupDataD env dt
                findidx 2 cs
            x -> error $ "smtci: contype: " ++ pretty x ++ " when lookup up " ++ pretty n

-- The tag index for a data type
smtti :: Integer
smtti = 1

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
smtT t = do
  tys <- gets ys_types
  case Map.lookup t tys of
     Just ty -> return ty
     Nothing -> do
        ty <- smtT' t
        modifyS $ \ys -> ys { ys_types = Map.insert t ty (ys_types ys) }
        return ty

-- | Compile a seri type to a smt type assuming it hasn't already been
-- compiled. Does not add the type to the types map (that's done by smtT).
smtT' :: Type -> CompilationM SMT.Type
smtT' t | Just (a, b) <- deArrowT t = SMT.ArrowT <$> mapM smtT [a, b] 
smtT' t | (ConT nm : args) <- unappsT t =
  let contype :: Con -> CompilationM (Maybe SMT.Type)
      contype (Con _ []) = return Nothing
      contype (Con _ ts) = Just . SMT.TupleT <$> mapM smtT ts
  in do
    poly <- gets ys_poly
    DataD _ vars vcs <- lookupDataD poly nm
    let n = mononametype t
    let yn = smtN n
    let assignments = (zip (map tyVarName vars) args)
    let cs = assign assignments vcs
    cts <- mapM contype cs
    let tag = SMT.DefineType (smttag n) (Just $ SMT.ScalarTD [smtN cn | Con cn _ <- cs])
    let ttype = SMT.TupleT (SMT.VarT (smttag n) : (catMaybes cts))
    let dt = SMT.DefineType yn (Just $ SMT.NormalTD ttype)
    let uidt = SMT.Define (smtuidt n) (SMT.VarT yn) Nothing
    addcmds [tag, dt, uidt]
    return (SMT.VarT yn)
smtT' t = throw $ "smtT: " ++ pretty t ++ " not supported"

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
     depat (ConP _ n []) e =
       let mypred = SMT.eqE (SMT.selectE e smtti) (SMT.varE (smtN n))
       in return ([mypred], [])
     depat (ConP _ n ps) e = do
       ci <- smtci n
       let ce = SMT.selectE e ci
       depats <- sequence [depat p (SMT.selectE ce i) | (p, i) <- zip ps [1..]]
       let (preds, binds) = unzip depats
       let mypred = SMT.eqE (SMT.selectE e smtti) (SMT.varE (smtN n))
       return (mypred:(concat preds), concat binds)
     depat (VarP (Sig n t)) e = return ([], [(pretty n, e)])
     depat (LitP (IntegerL i)) e = return ([SMT.eqE (SMT.integerE i) e], [])
     depat (LitP (CharL c)) e = return ([SMT.eqE (SMT.integerE (fromIntegral $ ord c)) e], [])
     depat (WildP _) _ = return ([], [])

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
       ((ConE s):args) -> yCon s args
       [VarE (Sig n t), _]
            | n == name "Seri.Lib.Prelude.error"
            , Just (_, dt) <- deArrowT t
            -> do errnm <- yfreeerr dt
                  return $ SMT.varE errnm
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.<" -> do   
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.ltE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.<=" -> do   
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.leqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.>" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.gtE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.&&" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.andE [a', b'])
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.||" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.orE [a', b'])
       [VarE (Sig n _), a] | n == name "Seri.Lib.Prelude.not" -> do
           a' <- smtE' a
           return (SMT.notE a')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_add_Integer" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.addE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_sub_Integer" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.subE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_mul_Integer" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.mulE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_eq_Integer" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.eqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_eq_Bit" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.eqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_add_Bit" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.bvaddE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_or_Bit" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.bvorE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_and_Bit" -> do
           a' <- smtE' a
           b' <- smtE' b
           return (SMT.bvandE a' b')
       -- TODO: should we allow shifting by an amount not statically
       -- determined? In that case, I think we need to convert the second
       -- argument to a bit vector in order to use smt bvshl function.
       [VarE (Sig n _), a, (LitE (IntegerL v))] | n == name "Seri.Lib.Bit.__prim_lsh_Bit" -> do
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
smtE' (ConE s) = yCon s []
smtE' (VarE (Sig n _)) = return $ SMT.varE (smtN n)


-- | Take the list of smt declarations made so far.
-- The returned list does not include any of the previously taken declarations
-- by calling smtD.
smtD :: CompilationM [SMT.Command]
smtD = do
  cmds <- gets $ reverse . ys_cmdsr
  modifyS $ \ys -> ys { ys_cmdsr = [] }
  return cmds

-- Give the monomorphic name for an applied type
mononametype :: Type -> Name
mononametype (ConT n) = n
mononametype (NumT n) = ntname n
mononametype (AppT a b) = mononametype a `nappend` name "$" `nappend` mononametype b

ntname :: NType -> Name
ntname n = name "#" `nappend` name (show (nteval n))
    
