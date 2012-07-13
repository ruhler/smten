
module Seri.Target.Yices.Yices2 (
    YS, ys, YCompiler, runYCompiler, yicesN, yicesE, yicesT, yicesD
    ) where

import qualified Yices2.Syntax as Y

import Control.Monad.State
import Data.Char(ord)
import Data.Maybe(catMaybes)

import Seri.Failable
import Seri.Lambda

runYCompiler :: YCompiler a -> YS -> Failable (a, YS)
runYCompiler = runStateT

ys :: Env -> YS
ys e = YS e [] 1 1 1

-- | Convert a seri name to a yices name.
yicesN :: String -> String
yicesN = yicesname

-- | Compile a seri expression to a yices expression.
-- The expression should be monomorphic.
-- Returns also a set of declarations that need to be made before using the
-- expression.
yicesE :: Exp -> YCompiler ([Y.Command], Y.Expression)
yicesE e = do
    e' <- yExp e
    cmds <- gets ys_cmds
    modify $ \ys -> ys { ys_cmds = [] }
    return (cmds, e')

-- | Compile a seri type to a yices type
-- The type should be monomorphic.
yicesT :: Type -> Failable Y.Type
yicesT = yType

-- | Compile a seri declarations to yices declarations.
-- The declarations should be monomorphic and in dependency order.
yicesD :: Dec -> YCompiler [Y.Command]
yicesD d = do
    d' <- yDec d
    cmds <- gets ys_cmds
    modify $ \ys -> ys { ys_cmds = [] }
    return (cmds ++ d')

data YS = YS {
    ys_env :: Env,          -- ^ The environment.
    ys_cmds :: [Y.Command], -- ^ Declarations needed for what was compiled
    ys_errid :: Integer,    -- ^ unique id to use for next free error variable
    ys_lamid :: Integer,    -- ^ unique id to use for next free lambda variable
    ys_caseid :: Integer    -- ^ unique id to use for next case arg variable
}

type YCompiler = StateT YS Failable

yfail :: String -> YCompiler a
yfail = lift . fail

-- Given the argument type and output type of a free error variable, return
-- the yices name of a newly defined one.
yfreeerr :: Type -> YCompiler String
yfreeerr t = do
    yt <- lift $ yType t
    id <- gets ys_errid
    let nm = yicesname ("err~" ++ show id)
    let cmd = Y.Define nm yt Nothing
    modify $ \ys -> ys { ys_cmds = cmd : ys_cmds ys, ys_errid = id+1 }
    return nm

-- Get a new, free variable for use as a case argument variable.
yfreecase :: YCompiler String
yfreecase = do
    id <- gets ys_caseid
    modify $ \ys -> ys { ys_caseid = id+1 }
    return $ yicesname ("c~" ++ show id)

-- Translate a seri expression to a yices expression
yExp :: Exp -> YCompiler Y.Expression
yExp (LitE (IntegerL x)) = return $ Y.integerE x
yExp (LitE (CharL c)) = return $ Y.integerE (fromIntegral $ ord c)
yExp e@(CaseE _ []) = yfail $ "empty case statement: " ++ pretty e
yExp (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.Expression -> YCompiler ([Y.Expression], [Y.Binding])
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
      dematch :: Y.Expression -> [Match] -> YCompiler Y.Expression
      dematch ye [] = do
          errnm <- yfreeerr (arrowsT [typeof e, typeof (head ms)])
          return $ Y.FunctionE (Y.varE errnm) [ye]
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- yExp b
          (preds, bindings) <- depat p e
          let pred = Y.andE preds
          let lete = if null bindings then b' else Y.LetE bindings b'
          return $ Y.ifE pred lete bms
  in do
      -- The expression e' can get really big, so we don't want to duplicate
      -- it when we use it to check for a pattern match in every alternative.
      -- Instead we bind it to variable ~c and duplicate that instead.
      e' <- yExp e
      cnm <- yfreecase
      body <- dematch (Y.varE cnm) ms
      return $ Y.LetE [(cnm, e')] body
yExp e@(AppE a b) =
    case unappsE e of 
       ((ConE s):args) -> yCon s args
       [VarE (Sig "error" _), _] -> do
           errnm <- yfreeerr (typeof e)
           return $ Y.varE errnm
       [VarE (Sig "<" _), a, b] -> do   
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.ltE a' b')
       [VarE (Sig ">" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.gtE a' b')
       [VarE (Sig "__prim_add_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.addE a' b')
       [VarE (Sig "__prim_sub_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           return (Y.subE a' b')
       [VarE (Sig "__prim_eq_Integer" _), a, b] -> do
           a' <- yExp a
           b' <- yExp b
           boxBool (Y.eqE a' b')
       _ -> do
           a' <- yExp a
           b' <- yExp b
           return $ Y.FunctionE a' [b']
yExp l@(LamE (Sig n xt) e) = do
    -- TODO: should we allow this or not?
    error $ "lambda expression in yices2 target generation: " ++ pretty l
    t <- lift $ yType (arrowsT [xt, typeof e])
    id <- gets ys_lamid
    let nm = yicesname ("lam~" ++ show id)
    let cmd = Y.Define nm t Nothing
    modify $ \ys -> ys { ys_cmds = cmd : ys_cmds ys, ys_lamid = id+1 }
    return (Y.varE nm)
yExp (ConE s) = yCon s []
yExp (VarE (Sig n _)) = return $ Y.varE (yicesname n)

-- Generate yices code for a fully applied constructor application.
yCon :: Sig -> [Exp] -> YCompiler Y.Expression
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

-- Given the name of a constructor, return the index for its fields in in the
-- data types tuple.
yicesci :: Name -> YCompiler Integer
yicesci n =
    let findidx :: Integer -> [Con] -> Failable Integer
        findidx _ [] = fail $ "index for " ++ n ++ " not found"
        findidx i ((Con cn []) : cs) = findidx i cs
        findidx i ((Con cn _) : _) | n == cn = return i
        findidx i (_ : cs) = findidx (i+1) cs
    in do
        env <- gets ys_env
        contype <- lift $ lookupDataConType env n
        case last $ unarrowsT contype of
            ConT dt -> do
                (DataD _ _ cs) <- lift $ lookupDataD env dt
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
yDec :: Dec -> YCompiler [Y.Command]
yDec (ValD (TopSig n [] t) _) = do
    -- TODO: should we allow this or not?
    error $ "Variable " ++ n ++ " has not been inlined"
    yt <- lift $ yType t
    return [Y.Define (yicesname n) yt Nothing]

yDec (DataD "Integer" _ _) =
    let deftype = Y.DefineType "Integer" (Just (Y.NormalTD Y.IntegerT))
    in return [deftype]

yDec (DataD "Char" _ _) =
    let deftype = Y.DefineType "Char" (Just (Y.NormalTD Y.IntegerT))
    in return [deftype]

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
        return [tag, dt, uidt]

yDec (PrimD (TopSig "<" _ _)) = return []
yDec (PrimD (TopSig ">" _ _)) = return []
yDec (PrimD (TopSig "__prim_add_Integer" _ _)) = return []
yDec (PrimD (TopSig "__prim_sub_Integer" _ _)) = return []
yDec (PrimD (TopSig "__prim_eq_Integer" _ _)) = return []
yDec (PrimD (TopSig "error" _ _)) = return []

yDec d = yfail $ "Cannot compile to yices: " ++ pretty d

-- box a bool into a Bool.
boxBool :: Y.Expression -> YCompiler Y.Expression
boxBool e = do
  true <- yExp trueE
  false <- yExp falseE
  return $ Y.ifE e true false

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

