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

module Seri.Target.Yices.Yices (
    Compilation(), compilation, CompilationM, runCompilation,
    yicesN, yicesT, yicesE, yicesD,
    ) where

import Debug.Trace

import qualified Yices.Syntax as Y

import qualified Data.Map as Map

import Data.List ((\\))
import Data.Char(ord)
import Data.Maybe(catMaybes)
import Data.Functor
import Control.Monad.State.Strict

import Seri.Failable
import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate

-- | A yices compilation object.
data Compilation = Compilation {
    ys_version :: Y.YicesVersion, -- ^ Version of yices to target
    ys_nocaseerr :: Bool,       -- ^ Assume an alternative will match in each case expression
    ys_poly :: Env,             -- ^ The polymorphic seri environment

    -- | map of already compiled types
    ys_types :: Map.Map Type Y.Type,

    -- | Declarations needed for what was compiled, stored in reverse order
    -- for efficiency sake.
    ys_cmdsr :: [Y.Command],

    ys_errid :: Integer,        -- ^ unique id to use for next free error variable
    ys_caseid :: Integer        -- ^ unique id to use for next case arg variable
}

-- | Monad for performing additional yices compilation.
type CompilationM = StateT Compilation Failable

-- | Append a list of commands in order to the commands specified so far.
addcmds :: [Y.Command] -> CompilationM ()
addcmds cmds = modifyS $ \ys -> ys { ys_cmdsr = (reverse cmds) ++ ys_cmdsr ys}

-- | Strict modify
modifyS :: (Compilation -> Compilation) -> CompilationM ()
modifyS f = do
    s <- get
    put $! f s

-- | Create a new yices compilation object.
compilation :: Y.YicesVersion    -- ^ yices version to target
               -> Bool         -- ^ nocase err?
               -> Env          -- ^ polymorphic seri environment
               -> Compilation
compilation version nocaseerr poly = Compilation {
    ys_version = version,
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

yfail :: String -> CompilationM a
yfail = lift . throw

-- Given the argument type and output type of a free error variable, return
-- the yices name of a newly defined one.
yfreeerr :: Type -> CompilationM String
yfreeerr t = do
    yt <- yicesT t
    id <- gets ys_errid
    let nm = yicesname ("err~" ++ show id)
    modifyS $ \ys -> ys { ys_errid = id+1 }
    addcmds [Y.Define nm yt Nothing]
    return nm

-- Get a new, free variable for use as a case argument variable.
yfreecase :: Char -> CompilationM String
yfreecase c = do
    id <- gets ys_caseid
    modifyS $ \ys -> ys { ys_caseid = id+1 }
    return $! yicesname (['c', c, '~'] ++ show id)

-- Generate yices code for a fully applied constructor application.
yCon :: Sig -> [Exp] -> CompilationM Y.Expression
yCon (Sig n _) [] | n == name "True" = return Y.trueE
yCon (Sig n _) [] | n == name "False" = return Y.falseE
yCon (Sig n ct) args = do
    let ConT dt = last $ unarrowsT ct
    let tagged = Y.tupleUpdateE (Y.varE $ yicesuidt dt) yicesti (Y.varE $ yicesN n)
    if null args
        then return tagged
        else do
            args' <- mapM yicesE args
            ci <- yicesci n
            return $ Y.tupleUpdateE tagged ci (Y.tupleE args')

-- Given the name of a data type, return an uninterpreted constant of that
-- type.
yicesuidt :: Name -> String
yicesuidt n = yicesname $ "uidt~" ++ pretty n

-- Given the name of a data type, return the name of it's tag type.
yicestag :: Name -> String
yicestag n = yicesname $ "tag~" ++ pretty n

-- Given the name of a constructor, return the index for its fields in the
-- data types tuple.
yicesci :: Name -> CompilationM Integer
yicesci n =
    let findidx :: Integer -> [Con] -> Failable Integer
        findidx _ [] = throw $ "index for " ++ pretty n ++ " not found"
        findidx i ((Con cn []) : cs) = findidx i cs
        findidx i ((Con cn _) : _) | n == cn = return i
        findidx i (_ : cs) = findidx (i+1) cs
    in do
        env <- gets ys_poly
        contype <- lift $ lookupDataConType env n
        case last $ unarrowsT contype of
            ConT dt -> do
                (DataD _ _ cs) <- lift $ lookupDataD env dt
                lift $ findidx 2 cs
            x -> error $ "yicesci: contype: " ++ pretty x ++ " when lookup up " ++ pretty n

-- The tag index for a data type
yicesti :: Integer
yicesti = 1

-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
yicesname :: String -> String
yicesname [] = []
yicesname "not" = "_not"
yicesname ('!':cs) = "__bang" ++ yicesname cs
yicesname ('#':cs) = "__hash" ++ yicesname cs
yicesname ('%':cs) = "__percent" ++ yicesname cs
yicesname ('&':cs) = "__amp" ++ yicesname cs
yicesname ('*':cs) = "__star" ++ yicesname cs
yicesname ('+':cs) = "__plus" ++ yicesname cs
yicesname ('.':cs) = "__dot" ++ yicesname cs
yicesname ('/':cs) = "__slash" ++ yicesname cs
yicesname ('<':cs) = "__lt" ++ yicesname cs
yicesname ('=':cs) = "__eq" ++ yicesname cs
yicesname ('>':cs) = "__gt" ++ yicesname cs
yicesname ('?':cs) = "__ques" ++ yicesname cs
yicesname ('@':cs) = "__at" ++ yicesname cs
yicesname ('\\':cs) = "__bslash" ++ yicesname cs
yicesname ('^':cs) = "__hat" ++ yicesname cs
yicesname ('|':cs) = "__bar" ++ yicesname cs
yicesname ('-':cs) = "__dash" ++ yicesname cs
yicesname ('(':cs) = "__oparen" ++ yicesname cs
yicesname (')':cs) = "__cparen" ++ yicesname cs
yicesname (',':cs) = "__comma" ++ yicesname cs
yicesname (c:cs) = c : yicesname cs

-- | Convert a seri name to a yices name.
yicesN :: Name -> String
yicesN = yicesname . unname

-- | Compile a seri type to a yices type
-- Before using the returned type, the yicesD function should be called to
-- get the required yices declarations.
yicesT :: Type -> CompilationM Y.Type
yicesT t | t == boolT = return Y.BoolT
yicesT t | t == integerT = return Y.IntegerT
yicesT t | t == charT = return Y.IntegerT
yicesT t | Just w <- deBitT t = return $ Y.BitVectorT w
yicesT t = do
  tys <- gets ys_types
  case Map.lookup t tys of
     Just ty -> return ty
     Nothing -> do
        ty <- yicesT' t
        modifyS $ \ys -> ys { ys_types = Map.insert t ty (ys_types ys) }
        return ty

-- | Compile a seri type to a yices type assuming it hasn't already been
-- compiled. Does not add the type to the types map (that's done by yicesT).
yicesT' :: Type -> CompilationM Y.Type
yicesT' t | Just (a, b) <- deArrowT t = Y.ArrowT <$> mapM yicesT [a, b] 
yicesT' t | (ConT nm : args) <- unarrowsT t =
  let contype :: Con -> CompilationM (Maybe Y.Type)
      contype (Con _ []) = return Nothing
      contype (Con _ ts) = Just . Y.TupleT <$> mapM yicesT ts
  in do
    poly <- gets ys_poly
    DataD _ vars vcs <- lift $ lookupDataD poly nm
    let n = mononametype t
    let yn = yicesN n
    let cs = assign (zip (map tyVarName vars) args) vcs
    cts <- mapM contype cs
    let tag = Y.DefineType (yicestag n) (Just $ Y.ScalarTD [yicesN cn | Con cn _ <- cs])
    let ttype = Y.TupleT (Y.VarT (yicestag n) : (catMaybes cts))
    let dt = Y.DefineType yn (Just $ Y.NormalTD ttype)
    let uidt = Y.Define (yicesuidt n) (Y.VarT yn) Nothing
    addcmds [tag, dt, uidt]
    return (Y.VarT yn)
yicesT' t = yfail $ "yicesT: " ++ pretty t ++ " not supported"

-- | Compile a seri expression to a yices expression.
-- Before using the returned expression, the yicesD function should be called
-- to get the required yices declarations.
yicesE :: Exp -> CompilationM Y.Expression
yicesE e | Just (VarP (Sig n _), v, x) <- deLet1E e = do
    v' <- yicesE v
    x' <- yicesE x
    return (Y.letE [(yicesN n, v')] x')

yicesE e | Just (p, a, b) <- deIfE e = do
  p' <- yicesE p
  a' <- yicesE a
  b' <- yicesE b
  return (Y.ifE p' a' b') 

yicesE e | Just (xs, ms) <- deCaseE e = do
  nocaseerr <- gets ys_nocaseerr
  (let -- depat p e
     --    outputs: (predicate, bindings)
     --   predicate - predicates indicating if the 
     --                pattern p matches expression e
     --   bindings - a list of bindings made when p matches e.
     depat :: Pat -> Y.Expression -> CompilationM ([Y.Expression], [Y.Binding])
     depat (ConP _ n []) e | n == name "True" = return ([e], [])
     depat (ConP _ n []) e | n == name "False" = return ([Y.notE e], [])
     depat (ConP _ n []) e =
       let mypred = Y.eqE (Y.selectE e yicesti) (Y.varE (yicesN n))
       in return ([mypred], [])
     depat (ConP _ n ps) e = do
       ci <- yicesci n
       let ce = Y.selectE e ci
       depats <- sequence [depat p (Y.selectE ce i) | (p, i) <- zip ps [1..]]
       let (preds, binds) = unzip depats
       let mypred = Y.eqE (Y.selectE e yicesti) (Y.varE (yicesN n))
       return (mypred:(concat preds), concat binds)
     depat (VarP (Sig n t)) e = return ([], [(pretty n, e)])
     depat (LitP (IntegerL i)) e = return ([Y.eqE (Y.integerE i) e], [])
     depat (LitP (CharL c)) e = return ([Y.eqE (Y.integerE (fromIntegral $ ord c)) e], [])
     depat (WildP _) _ = return ([], [])

     -- dematch es ms
     --    es - the expressions being cased on
     --    ms - the remaining matches in the case statement.
     --  outputs - the yices expression implementing the matches.
     dematch :: [Y.Expression] -> [Match] -> CompilationM Y.Expression
     dematch ye [] = do
         errnm <- yfreeerr (arrowsT $ map typeof xs ++ [typeof (head ms)])
         return $ Y.FunctionE (Y.varE errnm) ye
     dematch es [Match ps b] | nocaseerr = do
         b' <- yicesE b
         bindings <- concatMap snd <$> mapM (uncurry depat) (zip ps es)
         return $ Y.letE bindings b'
     dematch es ((Match ps b):ms) = do
         bms <- dematch es ms
         b' <- yicesE b
         (preds, bindings) <- unzip <$> mapM (uncurry depat) (zip ps es)
         let pred = Y.andE (concat preds)
         let lete = Y.letE (concat bindings) b'
         return $ Y.ifE pred lete bms

     givename :: (Y.Expression, Char) -> CompilationM ([Y.Binding], Y.Expression)
     givename (e@Y.ImmediateE {}, _) = return ([], e)
     givename (e, c) = do
        cnm <- yfreecase c
        return ([(cnm, e)], Y.varE cnm)
   in do
       -- The expressions e' can get really big, so we don't want to duplicate
       -- them when we use them it to check for a pattern match in every
       -- alternative. Instead we bind them to variables ~ca, ~cb, ... and
       -- duplicate that instead.
       es' <- mapM yicesE xs
       (binds, es'') <- unzip <$> mapM givename (zip es' "abcdefghijklmnopqrstuvwxyz")
       body <- dematch es'' ms
       return $ Y.letE (concat binds) body
   )
yicesE (AppE a []) = yicesE a
yicesE e@(AppE a b) =
    case unappsE e of 
       ((ConE s):args) -> yCon s args
       [VarE (Sig n t), _] | n == name "Seri.Lib.Prelude.error" -> do
           errnm <- yfreeerr t
           return $ Y.varE errnm
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.<" -> do   
           a' <- yicesE a
           b' <- yicesE b
           return (Y.ltE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.<=" -> do   
           a' <- yicesE a
           b' <- yicesE b
           return (Y.leqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.>" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.gtE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.&&" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.andE [a', b'])
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.||" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.orE [a', b'])
       [VarE (Sig n _), a] | n == name "Seri.Lib.Prelude.not" -> do
           a' <- yicesE a
           return (Y.notE a')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_add_Integer" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.addE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_sub_Integer" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.subE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_mul_Integer" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.mulE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Prelude.__prim_eq_Integer" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.eqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_eq_Bit" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.eqE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_add_Bit" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.bvaddE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_or_Bit" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.bvorE a' b')
       [VarE (Sig n _), a, b] | n == name "Seri.Lib.Bit.__prim_and_Bit" -> do
           a' <- yicesE a
           b' <- yicesE b
           return (Y.bvandE a' b')
       -- TODO: should we allow shifting by an amount not statically
       -- determined? In that case, I think we need to convert the second
       -- argument to a bit vector in order to use yices bvshl function.
       [VarE (Sig n _), a, (LitE (IntegerL v))] | n == name "Seri.Lib.Bit.__prim_lsh_Bit" -> do
           a' <- yicesE a
           return (Y.bvshiftLeft0E a' v)
       [VarE (Sig n _), a, (LitE (IntegerL v))] | n == name "Seri.Lib.Bit.__prim_rshl_Bit" -> do
           a' <- yicesE a
           return (Y.bvshiftRight0E a' v)
       [VarE (Sig n bt), LitE (IntegerL x)]
            | n == name "Seri.Lib.Bit.__prim_fromInteger_Bit"
            , Just w <- deBitT bt
            -> return (Y.mkbvE w x)
       [VarE (Sig n t), a]
            | n == name "Seri.Lib.Bit.__prim_zeroExtend_Bit"
            , Just (bs, bt) <- deArrowT t
            , Just sw <- deBitT bs
            , Just tw <- deBitT bt
            -> do
               a' <- yicesE a
               return (Y.bvzeroExtendE a' (tw - sw))
       [VarE (Sig n t), a]
            | n == name "Seri.Lib.Bit.__prim_truncate_Bit"
            , Just (_, bt) <- deArrowT t
            , Just tw <- deBitT bt
            -> Y.bvextractE (tw - 1) 0 <$> yicesE a
       [VarE (Sig n _), x, LitE (IntegerL i)]
            | n == name "Seri.Lib.Bit.__prim_extract_Bit"
            , Just tw <- deBitT (typeof e)
            -> Y.bvextractE (i + tw - 1) i <$> yicesE x
       [VarE (Sig n _), f, k, v] | n == name "Seri.SMT.Array.update" -> do
           f' <- yicesE f
           k' <- yicesE k
           v' <- yicesE v
           return $ Y.UpdateE f' [k'] v'
       _ -> do
           a' <- yicesE (AppE a (init b))
           b' <- yicesE (last b)
           return $ Y.FunctionE a' [b']
yicesE (LitE (IntegerL x)) = return $ Y.integerE x
yicesE (LitE (CharL c)) = return $ Y.integerE (fromIntegral $ ord c)
yicesE l@(LaceE ms) = 
    yfail $ "lambda expression in yices target generation: " ++ pretty l
yicesE (ConE s) = yCon s []
yicesE (VarE (Sig n _)) = return $ Y.varE (yicesN n)


-- | Take the list of yices declarations made so far.
-- The returned list does not include any of the previously taken declarations
-- by calling yicesD.
yicesD :: CompilationM [Y.Command]
yicesD = do
  cmds <- gets $ reverse . ys_cmdsr
  modifyS $ \ys -> ys { ys_cmdsr = [] }
  return cmds

