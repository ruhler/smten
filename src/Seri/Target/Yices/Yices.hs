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

module Seri.Target.Yices.Yices (
    Compilation(), compilation, CompilationM, runCompilation,
    yicesN, yicesT, yicesE,
    ) where

import Debug.Trace

import qualified Yices.Syntax as Y

import Data.List ((\\))
import Data.Char(ord)
import Data.Maybe(catMaybes)
import Control.Monad.State.Strict

import Seri.Failable
import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate
import Seri.Target.Inline

-- | A yices compilation object.
data Compilation = Compilation {
    ys_version :: Y.YicesVersion, -- ^ Version of yices to target
    ys_idepth :: Integer,       -- ^ Depth of inlining to perform
    ys_poly :: Env,             -- ^ The polymorphic seri environment
    ys_mono :: [Dec],           -- ^ Already declared (monomorphic) declarations
    ys_monoe :: Env,            -- ^ The environment corresponding to ys_mono

    -- | Declarations needed for what was compiled, stored in reverse order
    -- for efficiency sake.
    ys_cmdsr :: [Y.Command],
    ys_errid :: Integer,        -- ^ unique id to use for next free error variable
    ys_caseid :: Integer        -- ^ unique id to use for next case arg variable
}

-- Get the commands in forward order.
ys_cmds :: Compilation -> [Y.Command]
ys_cmds = reverse . ys_cmdsr

-- | Monad for performing additional yices compilation.
type CompilationM = StateT Compilation Failable

-- When a compilation fails, append the given string to the error message.
confail :: String -> CompilationM a -> CompilationM a
confail m x = do
    s <- get
    (v, s') <- lift $ onfail (\msg -> fail $ msg ++ "\n" ++ m) $ runStateT x s
    put s'
    return v

-- | Strict gets.
getsS :: (Compilation -> a) -> CompilationM a
getsS f = do
    s <- get
    return $! f s

-- | Strict modify
modifyS :: (Compilation -> Compilation) -> CompilationM ()
modifyS f = do
    s <- get
    put $! f s

-- | Create a new yices compilation object.
compilation :: Y.YicesVersion    -- ^ yices version to target
               -> Integer      -- ^ inline depth
               -> Env          -- ^ polymorphic seri environment
               -> Compilation
compilation version idepth poly = Compilation {
    ys_version = version,
    ys_idepth = idepth,
    ys_poly = tweak tweakings poly,
    ys_mono = [],
    ys_monoe = mkEnv [],
    ys_cmdsr = [],
    ys_errid = 1,
    ys_caseid = 1 
}

-- | Convert a seri name to a yices name.
yicesN :: String -> String
yicesN = yicesname

-- | Compile a seri type to a yices type
-- Returns a list of yices commands needed to use the compiled type.
yicesT :: Type -> CompilationM ([Y.Command], Y.Type)
yicesT t = do
   modifyS $ \ys -> ys { ys_cmdsr = [] }
   mt <- compileNeeded t 
   cmds <- getsS ys_cmds
   yt <- lift $ yType mt
   return (cmds, yt)

-- | Compile a seri expression to a yices expression.
-- Returns a list of yices commands needed to use the compiled expressions.
yicesE :: Exp -> CompilationM ([Y.Command], Y.Expression)
yicesE e = do
    idepth <- getsS ys_idepth
    poly <- getsS ys_poly
    let se = elaborate SNF poly e
    modifyS $ \ys -> ys { ys_cmdsr = [] }
    me <- confail ("When compiling " ++ pretty se) $ compileNeeded se
    ye <- confail ("When compiling " ++ pretty se) $ yExp me
    cmds <- getsS ys_cmds
    return (cmds, ye)

-- | Run a compilation.
runCompilation :: CompilationM a -> Compilation -> Failable (a, Compilation)
runCompilation = runStateT

yfail :: String -> CompilationM a
yfail = lift . fail

-- | Append a list of commands in order to the commands specified so far.
addcmds :: [Y.Command] -> CompilationM ()
addcmds cmds = modifyS $ \ys -> ys { ys_cmdsr = (reverse cmds) ++ ys_cmdsr ys}

-- Given some object, compile everything in the environment needed for this
-- object, and return the monomorphic object.
compileNeeded :: (Monomorphic a, Ppr a) => a -> CompilationM a
compileNeeded x = do
    poly <- getsS ys_poly
    let (mdecs, mx) = monomorphic poly x
    let (sorted, r) = sort mdecs
    if null r
        then return ()  
        else yfail $ "yices recursive declarations not supported: "
                ++ pretty r ++ ",\n needed for " ++ pretty x
    decl <- getsS ys_mono
    let ndecl = sorted \\ decl
    let nmono = decl ++ ndecl
    -- TODO: should we use "tweak" to update ys_monoe?
    modifyS $ \ys -> ys { ys_mono = nmono, ys_monoe = mkEnv nmono}
    mapM_ yDec ndecl
    return mx


-- Given the argument type and output type of a free error variable, return
-- the yices name of a newly defined one.
yfreeerr :: Type -> CompilationM String
yfreeerr t = do
    yt <- lift $ yType t
    id <- getsS ys_errid
    let nm = yicesname ("err~" ++ show id)
    modifyS $ \ys -> ys { ys_errid = id+1 }
    addcmds [Y.Define nm yt Nothing]
    return nm

-- Get a new, free variable for use as a case argument variable.
yfreecase :: CompilationM String
yfreecase = do
    id <- getsS ys_caseid
    modifyS $ \ys -> ys { ys_caseid = id+1 }
    return $! yicesname ("c~" ++ show id)

-- Translate a seri expression to a yices expression
yExp :: Exp -> CompilationM Y.Expression
yExp (LitE (IntegerL x)) = return $ Y.integerE x
yExp (LitE (CharL c)) = return $ Y.integerE (fromIntegral $ ord c)
yExp e@(CaseE _ []) = yfail $ "empty case statement: " ++ pretty e
yExp (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.Expression -> CompilationM ([Y.Expression], [Y.Binding])
      depat (ConP _ n []) e =
        let mypred = Y.eqE (Y.selectE e yicesti) (Y.varE (yicesname n))
        in return ([mypred], [])
      depat (ConP _ n ps) e = do
        ci <- yicesci n
        let ce = Y.selectE e ci
        depats <- sequence [depat p (Y.selectE ce i) | (p, i) <- zip ps [1..]]
        let (preds, binds) = unzip depats
        let mypred = Y.eqE (Y.selectE e yicesti) (Y.varE (yicesname n))
        return (mypred:(concat preds), concat binds)
      depat (VarP (Sig n t)) e = return ([], [(n, e)])
      depat (IntegerP i) e = return ([Y.eqE (Y.integerE i) e], [])
      depat (WildP _) _ = return ([], [])

      -- dematch e ms
      --    e - the expression being cased on
      --    ms - the remaining matches in the case statement.
      --  outputs - the yices expression implementing the matches.
      dematch :: Y.Expression -> [Match] -> CompilationM Y.Expression
      dematch ye [] = do
          errnm <- yfreeerr (arrowsT [typeof e, typeof (head ms)])
          return $ Y.FunctionE (Y.varE errnm) [ye]
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- yExp b
          (preds, bindings) <- depat p e
          let pred = Y.andE preds
          let lete = if null bindings then b' else yLetE bindings b'
          return $ Y.ifE pred lete bms
  in do
      -- The expression e' can get really big, so we don't want to duplicate
      -- it when we use it to check for a pattern match in every alternative.
      -- Instead we bind it to variable ~c and duplicate that instead.
      e' <- yExp e
      case e' of
         Y.ImmediateE {} -> dematch e' ms
         _ -> do
            cnm <- yfreecase
            body <- dematch (Y.varE cnm) ms
            return $ yLetE [(cnm, e')] body
yExp (VarE (Sig "~error" t)) = do
    errnm <- yfreeerr t
    return $ Y.varE errnm
yExp e@(AppE a b) =
    case unappsE e of 
       ((ConE s):args) -> yCon s args
       [VarE (Sig "Seri.Lib.Prelude.<" _), a, b] -> do   
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.ltE a' b')
       [VarE (Sig "Seri.Lib.Prelude.>" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.gtE a' b')
       [VarE (Sig "Seri.Lib.Prelude.__prim_add_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.addE a' b')
       [VarE (Sig "Seri.Lib.Prelude.__prim_sub_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.subE a' b')
       [VarE (Sig "Seri.Lib.Prelude.__prim_mul_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.mulE a' b')
       [VarE (Sig "Seri.Lib.Prelude.__prim_eq_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.eqE a' b')
       [VarE (Sig "Seri.Lib.Bit.__prim_eq_Bit" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.eqE a' b')
       [VarE (Sig "Seri.Lib.Bit.__prim_add_Bit" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.bvaddE a' b')
       [VarE (Sig "Seri.Lib.Bit.__prim_or_Bit" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.bvorE a' b')
       [VarE (Sig "Seri.Lib.Bit.__prim_and_Bit" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.bvandE a' b')
       -- TODO: should we allow shifting by an amount not statically
       -- determined? In that case, I think we need to convert the second
       -- argument to a bit vector in order to use yices bvshl function.
       [VarE (Sig "Seri.Lib.Bit.__prim_lsh_Bit" _), a, (LitE (IntegerL v))] -> do
           a' <- yExp a
           return (Y.bvshiftLeft0E a' v)
       [VarE (Sig "Seri.Lib.Bit.__prim_rshl_Bit" _), a, (LitE (IntegerL v))] -> do
           a' <- yExp a
           return (Y.bvshiftRight0E a' v)
       [VarE (Sig "Seri.Lib.Bit.__prim_fromInteger_Bit" (AppT _ (ConT ('B':'i':'t':'$':'#':v)))), LitE (IntegerL x)] -> do
           return (Y.mkbvE (read v) x)
       [VarE (Sig "Seri.Lib.Bit.__prim_zeroExtend_Bit" (AppT (AppT _ (ConT ('B':'i':'t':'$':'#':sw))) (ConT ('B':'i':'t':'$':'#':tw)))), a] -> do
           a' <- yExp a
           return (Y.bvzeroExtendE a' (read tw - read sw))
       [VarE (Sig "Seri.Lib.Bit.__prim_trucate_Bit" (AppT (AppT _ (ConT ('B':'i':'t':'$':'#':sw))) (ConT ('B':'i':'t':'$':'#':tw)))), a] -> do
           a' <- yExp a
           return (Y.bvextractE a' 0 (read tw - 1))
       [VarE (Sig "Seri.SMT.Array.update" _), f, k, v] -> do
           f' <- yExp f
           k' <- yExp k
           v' <- yExp v
           return $ Y.UpdateE f' [k'] v'
       [LamE (Sig n _) b, arg] -> do
           arg' <- yExp arg
           b' <- yExp b
           return $ yLetE [(n, arg')] b'
       _ -> do
           a' <- yExp a
           b' <- yExp b
           return $ Y.FunctionE a' [b']
yExp l@(LamE (Sig n xt) e) = 
    yfail $ "lambda expression in yices target generation: " ++ pretty l
yExp (ConE s) = yCon s []
yExp (VarE (Sig n _)) = return $ Y.varE (yicesname n)

-- Let expression in yices.
-- Tries to do simplification of the let so queries don't look quite so ugly.
yLetE :: [Y.Binding] -> Y.Expression -> Y.Expression
yLetE [] e = e
yLetE [(n, e)] (Y.ImmediateE (Y.VarV n')) | n == n' = e
yLetE bs e = Y.LetE bs e

-- Generate yices code for a fully applied constructor application.
yCon :: Sig -> [Exp] -> CompilationM Y.Expression
yCon (Sig n ct) args = do
    let ConT dt = last $ unarrowsT ct
    let tagged = Y.tupleUpdateE (Y.varE $ yicesuidt dt) yicesti (Y.varE $ yicesname n)
    if null args
        then return tagged
        else do
            args' <- mapM yExp args
            ci <- yicesci n
            return $ Y.tupleUpdateE tagged ci (Y.tupleE args')

-- Given the name of a data type, return an uninterpreted constant of that
-- type.
yicesuidt :: Name -> Name
yicesuidt n = yicesname $ "uidt~" ++ n

-- Given the name of a data type, return the name of it's tag type.
yicestag :: Name -> Name
yicestag n = yicesname $ "tag~" ++ n

-- Given the name of a constructor, return the index for its fields in the
-- data types tuple.
yicesci :: Name -> CompilationM Integer
yicesci n =
    let findidx :: Integer -> [Con] -> Failable Integer
        findidx _ [] = fail $ "index for " ++ n ++ " not found"
        findidx i ((Con cn []) : cs) = findidx i cs
        findidx i ((Con cn _) : _) | n == cn = return i
        findidx i (_ : cs) = findidx (i+1) cs
    in do
        envr <- getsS ys_monoe
        contype <- lift $ lookupDataConType envr n
        case last $ unarrowsT contype of
            ConT dt -> do
                (DataD _ _ cs) <- lift $ lookupDataD envr dt
                lift $ findidx 2 cs
            x -> error $ "yicesci: contype: " ++ pretty x ++ " when lookup up " ++ n

-- The tag index for a data type
yicesti :: Integer
yicesti = 1

yType :: Type -> Failable Y.Type
yType (ConT n) = return $ Y.VarT (yicesname n)
yType (AppT (AppT (ConT "->") a) b) = do
    a' <- yType a
    b' <- yType b
    return $ Y.ArrowT [a', b']
yType t = fail $ "Cannot compile to yices: " ++ pretty t

-- yDec
--   Assumes the declaration is monomorphic.
yDec :: Dec -> CompilationM ()
yDec (ValD (TopSig n [] t) _) = do
    -- TODO: should we allow this or not?
    error $ "Variable " ++ n ++ " has not been inlined"

yDec (DataD "Integer" _ _) =
    let deftype = Y.DefineType "Integer" (Just (Y.NormalTD Y.IntegerT))
    in addcmds [deftype]

yDec (DataD "Char" _ _) =
    let deftype = Y.DefineType "Char" (Just (Y.NormalTD Y.IntegerT))
    in addcmds [deftype]

yDec (DataD bv@('B':'i':'t':'$':'#':v) _ _) =
    let deftype = Y.DefineType (yicesname bv) (Just (Y.NormalTD (Y.BitVectorT (read v))))
    in addcmds [deftype]

yDec (DataD n [] cs) =
    let conname :: Con -> String
        conname (Con n _) = yicesname n

        contype :: Con -> Failable (Maybe Y.Type)
        contype (Con _ []) = return Nothing
        contype (Con _ ts) = do 
            ts' <- mapM yType ts
            return $ Just (Y.TupleT ts')
    in do
        cts <- lift $ mapM contype cs
        let tag = Y.DefineType (yicestag n) (Just $ Y.ScalarTD (map conname cs))
        let ttype = Y.TupleT (Y.VarT (yicestag n) : (catMaybes cts))
        let dt = Y.DefineType (yicesname n) (Just $ Y.NormalTD ttype)
        let uidt = Y.Define (yicesuidt n) (Y.VarT (yicesname n)) Nothing
        addcmds [tag, dt, uidt]

yDec (PrimD (TopSig "Seri.Lib.Prelude.<" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Prelude.>" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Prelude.__prim_add_Integer" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Prelude.__prim_sub_Integer" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Prelude.__prim_mul_Integer" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Prelude.__prim_eq_Integer" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_add_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_sub_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_mul_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_eq_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_fromInteger_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_zeroExtend_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_trucate_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_lsh_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_rshl_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_or_Bit" _ _)) = return ()
yDec (PrimD (TopSig "Seri.Lib.Bit.__prim_and_Bit" _ _)) = return ()
yDec (PrimD (TopSig "~error" _ _)) = return ()
yDec (PrimD (TopSig "Seri.SMT.Array.update" _ _)) = return ()

yDec d = yfail $ "Cannot compile to yices: " ++ pretty d

-- box a bool into a Bool.
boxBool :: Y.Expression -> CompilationM Y.Expression
boxBool e = do
  true <- yExp trueE
  false <- yExp falseE
  return $ Y.ifE e true false


-- Changes to the (polymorphic) declarations to handle yices specific things.
-- The primitive error turns into:
--    error _ = ~error
-- This way we dont need to support strings in yices to use error.
tweakings :: [Dec]
tweakings =
  let body = clauseE [Clause [WildP stringT] (VarE (Sig "~error" (VarT "a")))]
      t = arrowsT [listT charT, VarT "a"]
      errorv = ValD (TopSig "Seri.Lib.Prelude.error" [] t) body
      errorp = PrimD (TopSig "~error" [] (VarT "a"))
  in [errorv, errorp]

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

