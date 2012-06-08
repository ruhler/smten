
module Seri.Target.Yices.Yices (yicesY, compile_decs) where

import Data.Maybe(fromJust, fromMaybe)
import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Builtins.Prelude
import Seri.Utils.Ppr

-- Translate a seri expression to a yices expression
yExp :: Compiler -> Exp -> Maybe ([Y.CmdY], Y.ExpY)
yExp _ (IntegerE x) = Just $ ([], Y.LitI x)
yExp c (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.ExpY -> ([Y.ExpY], [((String, Maybe Y.TypY), Y.ExpY)])
      depat (ConP (Sig n _) ps) e =
        let (preds, binds) = unzip [depat p (Y.APP (Y.VarE (n ++ show i)) [e])
                                    | (p, i) <- zip ps [0..]]
            mypred = Y.APP (Y.VarE (n ++ "?")) [e]
        in (mypred:(concat preds), concat binds)
      depat (VarP (Sig n t)) e = ([], [((n, compile_type c c t), e)])
      depat (IntegerP i) e = ([Y.LitI i Y.:= e], [])
      depat (WildP _) _ = ([], [])

      -- take the AND of a list of predicates in a reasonable way.
      yand :: [Y.ExpY] -> Y.ExpY
      yand [] = Y.VarE "true"
      yand [x] = x
      yand xs = Y.AND xs

      -- take the OR of a list of predicats in a reasonable way.
      yor :: [Y.ExpY] -> Y.ExpY
      yor [] = Y.VarE "false"
      yor [x] = x
      yor xs = Y.OR xs

      -- dematch e ms
      --    e - the expression being cased on
      --    ms - the remaining matches in the case statement.
      --  outputs (cmds, pred, body)
      --    cmds - additional commands to be executed.
      --    pred - predicates for each match.
      --    body - the yices expression implementing the matches.
      dematch :: Y.ExpY -> [Match] -> ([Y.CmdY], [Y.ExpY], Y.ExpY)
      dematch e [] = error $ "empty case statement"
      dematch e [Match p b] = 
        let Just (cmds, b') = compile_exp c c b
            (pred, bindings) = depat p e
        in (cmds, [yand pred], Y.LET bindings b')
      dematch e ((Match p b):ms) =
        let (cms, pms, bms) = dematch e ms
            Just (cmds, b') = compile_exp c c b
            (preds, bindings) = depat p e
            pred = yand preds
        in (cmds ++ cms, pred:pms, Y.IF pred (Y.LET bindings b') bms)
  in do
      (es, e') <- compile_exp c c e
      let (cmds, ps, b) = dematch e' ms
      return (cmds ++ [Y.ASSERT (yor ps)], b)
yExp c (AppE a b) = do
    (as, a') <- compile_exp c c a
    (bs, b') <- compile_exp c c b
    return (as ++ bs, Y.APP a' [b'])
yExp c (LamE (Sig n t) e) = do
    (es, e') <- compile_exp c c e
    return (es, Y.LAMBDA [(n, fromJust $ compile_type c c t)] e')
yExp _ (ConE (Sig n _)) = return ([], Y.VarE (yicesname n))
yExp _ (VarE (Sig n _) _) = return ([], Y.VarE (yicesname n))
yExp _ _ = Nothing

-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
--
-- I don't have documentation for what yices allows in names, but it appears
-- symbols aren't allowed. So this just replaces each symbol with an ascii
-- approximation.
yicesname :: String -> String
yicesname [] = []
yicesname ('!':cs) = "__bang" ++ yicesname cs
yicesname ('#':cs) = "__hash" ++ yicesname cs
yicesname ('$':cs) = "__dollar" ++ yicesname cs
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
yicesname ('~':cs) = "__tilde" ++ yicesname cs
yicesname (c:cs) = c : yicesname cs

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ (ConT n) = Just $ Y.VarT n
yType c (AppT (AppT (ConT "->") a) b) = do
    a' <- compile_type c c a
    b' <- compile_type c c b
    return $ Y.ARR [a', b']
yType _ _ = Nothing

coreY :: Compiler
coreY = Compiler [] yExp yType

yicesY :: Compiler
yicesY = compilers [preludeY, coreY]
            
-- compile_dec
--   Assumes the declaration is monomorphic.
compile_dec :: Compiler -> Dec -> [Y.CmdY]
compile_dec c (ValD (Sig n t) e) =
    let yt = fromMaybe (error $ "compile type " ++ render (ppr t)) (compile_type c c t)
        (cmds, ye) = fromMaybe (error $ "compile exp " ++ render (ppr e)) (compile_exp c c e)
    in cmds ++ [Y.DEFINE (yicesname n, yt) (Just ye)]
compile_dec c (DataD n [] cs) =
    let con :: Con -> (String, [(String, Y.TypY)])
        con (Con n ts) = (n, zip [n ++ show i | i <- [0..]]
                                 (map (fromJust . compile_type c c) ts))
    in [Y.DEFTYP n (Just (Y.DATATYPE (map con cs)))]
compile_dec c d
    = error $ "compile_dec: cannot compile to yices: " ++ render (ppr d)

compile_decs :: Compiler -> [Dec] -> [Y.CmdY]
compile_decs c ds = concat $ map (compile_dec c) ds

