
module Seri.Target.Yices.Yices2 (
    YS, ys, YCompiler, runYCompiler, yicesN, yicesE, yicesT, yicesD
    ) where

import qualified Yices2.Syntax as Y

import Control.Monad.State
import Data.Maybe(catMaybes)

import Seri.Failable
import Seri.Lambda

runYCompiler :: YCompiler a -> YS -> Failable (a, YS)
runYCompiler = runStateT

ys :: YS
ys = YS [] 1

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
    ys_cmds :: [Y.Command], -- ^ Declarations needed for what was compiled
    ys_errid :: Integer     -- ^ unique id to use for next free error variable
    ys_lamid :: Integer     -- ^ unique id to use for next free lambda variable
}

type YCompiler = StateT YS Failable

yfail :: String -> YCompiler a
yfail = lift . fail

-- Given the argument type and output type of a free error variable, return
-- the yices name of a newly defined one.
yfreeerr :: Type -> Type -> YCompiler String
yfreeerr it ot = do
    t <- lift $ yType (arrowsT [it, ot])
    id <- gets ys_errid
    let nm = yicesname ("err~" ++ show id)
    let cmd = Y.Define nm t Nothing
    modify $ \ys -> ys { ys_cmds = cmd : ys_cmds ys, ys_errid = id+1 }
    return nm

-- Translate a seri expression to a yices expression
yExp :: Exp -> YCompiler Y.Expression
yExp (IntegerE x) = return $ Y.integerE x
yExp e@(CaseE _ []) = yfail $ "empty case statement: " ++ pretty e
yExp (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.Expression -> ([Y.Expression], [Y.Binding])
      depat (ConP _ n []) e =
        let mypred = Y.eqE (Y.selectE e 0) (Y.varE (yicesname n))
        in ([mypred], [])
      depat (ConP _ n ps) e =
        let ce = Y.selectE e (Y.varE $ yicesci n)
            (preds, binds) = unzip [depat p (Y.selectE ce i) | (p, i) <- zip ps [1..]]
            mypred = Y.eqE (Y.selectE e 0) (Y.varE (yicesname n))
        in (mypred:(concat preds), concat binds)
      depat (VarP (Sig n t)) e =
        let 
        in ([], [(n, e)])
      depat (IntegerP i) e = ([Y.eqE (Y.integerE i) e], [])
      depat (WildP _) _ = ([], [])

      -- dematch e ms
      --    e - the expression being cased on
      --    ms - the remaining matches in the case statement.
      --  outputs - the yices expression implementing the matches.
      dematch :: Y.Expression -> [Match] -> YCompiler Y.Expression
      dematch ye [] = do
          errnm <- yfreeerr (typeof e) (typeof (head ms))
          return $ Y.FunctionE (Y.varE errnm) [ye]
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- yExp b
          let (preds, bindings) = depat p e
          let pred = Y.andE preds
          return $ Y.ifE pred (Y.LetE bindings b') bms
  in do
      e' <- yExp e
      dematch e' ms
yExp e@(AppE a b) = do
    (f:args) = unappsE e
    case f of 
       ConE s -> yCon s args
       _ -> do
           a' <- yExp a
           b' <- yExp b
           return $ Y.FunctionE a' [b']
yExp e@(LamE (Sig n xt) e) = do
    t <- lift $ yType (arrowsT [xt, typeof e])
    id <- gets ys_lamid
    let nm = yicesname ("lam~" ++ show id)
    let cmd = Y.Define nm t Nothing
    modify $ \ys -> ys { ys_cmds = cmd : ys_cmds ys, ys_lamid = id+1 }
    return (Y.varE nm)
yExp (ConE {} s) = yCon s []
yExp (VarE (Sig n _)) = return $ Y.varE (yicesname n)

-- Generate yices code for a fully applied constructor application.
yCon :: Sig -> [Exp] -> YCompiler Y.Expression
yCon (Sig n (ConT dt)) args = do
    let tagged = tupleUpdateE (Y.varE $ yicesuidt dt) 0 (yicesname n)
    if null args
        then return tagged
        else do
            args' <- mapM yExp args
            return $ tupleUpdateE tagged (Y.varE $ yicesci n) (tupleE args')

-- Given the name of a data type, return an uninterpreted constant of that
-- type.
yicesuidt :: Name -> Name
yicesuidt n = yicesname $ "uidt~" ++ n

-- Given the name of a data type, return the name of it's tag type.
yicestag :: Name -> Name
yicestag n = yicesname $ "tag~" ++ n

-- Given the name of a constructor, return the name of an integer with its
-- index in the data types tuple.
yicesci :: Name -> Name
yicesci n = yicesname $ "idx~" ++ n

yType :: Type -> Failable Y.Type
yType (ConT n) = return $ Y.VarT (yicesname n)
yType (AppT (AppT (ConT "->") a) b) = do
    a' <- yType a
    b' <- yType b
    return $ Y.ArrowT [a', b']
yType t = fail $ "Cannot compile to yices: " ++ pretty t

-- yDec
--   Assumes the declaration is monomorphic.
yDec :: Dec -> YCompiler [Y.CmdY]
yDec (ValD (TopSig n [] t) _) = do
    yt <- lift $ yType t
    return [Y.Define (yicesname n) yt Nothing]

yDec (DataD "Integer" _ _) =
    let deftype = Y.DefineType "Integer" (Just (Y.NormalTD (Y.VarT "int")))
    in return [deftype]

yDec (DataD n [] cs) =
    let conname :: Con -> String
        conname (Con n _) = n

        contype :: Con -> Failable (Maybe Y.Type)
        contype (Con _ []) = return Nothing
        contype (Con _ ts) = do 
            ts' <- map yType ts
            return $ Y.TupleT ts'

        conidxs :: Integer -> [Con] -> [Y.Command]
        conidxs _ [] = []
        conidxs i (Con n [] : cs) = conidxs i cs
        conidxs i (Con n _ : cs) =
            (Y.Define (yicesci n) (Just $ Y.integerE i)) : conidxs (i+1) cs
    in do
        cts <- mapM contype cs
        let tag = Y.DefineType (yicestag n) (Just $ Y.ScalarTD (map conname cs))
        let ttype = Y.TupleT (Y.varT tag : (catMaybes cts))
        let dt = Y.DefineType (yicesname n) (Just $ Y.NormalTD ttype)
        let uidt = Y.Define (yicesuidt n) (Y.VarT n) Nothing
        return $ [tag, dt, uidt] ++ conidxs 1 cs

yDec d = yfail $ "Cannot compile to yices: " ++ pretty d

-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
yicesname :: String -> String
yicesname [] = []
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

