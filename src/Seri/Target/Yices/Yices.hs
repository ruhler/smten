
module Seri.Target.Yices.Yices (yicesY, compile_decs) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Builtins.Prelude

-- Translate a seri expression to a yices expression
yExp :: Compiler -> Exp -> YCM Y.ExpY
yExp _ (IntegerE x) = return $ Y.LitI x
yExp _ e@(CaseE _ []) = fail $ "empty case statement: " ++ pretty e
yExp c (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.ExpY -> ([Y.ExpY], [((String, Maybe Y.TypY), Y.ExpY)])
      depat (ConP _ n ps) e =
        let (preds, binds) = unzip [depat p (Y.APP (Y.VarE (yicesname n ++ show i)) [e])
                                    | (p, i) <- zip ps [0..]]
            mypred = Y.APP (Y.VarE (yicesname n ++ "?")) [e]
        in (mypred:(concat preds), concat binds)
      depat (VarP (Sig n t)) e = ([], [((n, runYCM $ compile_type c c t), e)])
      depat (IntegerP i) e = ([Y.LitI i Y.:= e], [])
      depat (WildP _) _ = ([], [])

      -- take the AND of a list of predicates in a reasonable way.
      yand :: [Y.ExpY] -> Y.ExpY
      yand [] = Y.VarE "true"
      yand [x] = x
      yand xs = Y.AND xs

      -- dematch e ms
      --    e - the expression being cased on
      --    ms - the remaining matches in the case statement.
      --  outputs - the yices expression implementing the matches.
      dematch :: Y.ExpY -> [Match] -> YCM Y.ExpY
      dematch e [Match p b] = do
          -- TODO: return an error condition of some sort if 
          -- the predicate for the last match doesn't hold.
          b' <- compile_exp c c b
          let (pred, bindings) = depat p e
          return $ Y.LET bindings b'
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- compile_exp c c b
          let (preds, bindings) = depat p e
          let pred = yand preds
          return $ Y.IF pred (Y.LET bindings b') bms
  in do
      e' <- compile_exp c c e
      dematch e' ms
yExp c (AppE a b) = do
    a' <- compile_exp c c a
    b' <- compile_exp c c b
    return $ Y.APP a' [b']
yExp c (LamE (Sig n t) e) = do
    e' <- compile_exp c c e
    return $ Y.LAMBDA [(n, fromYCM $ compile_type c c t)] e'
yExp _ (ConE (Sig n _)) = return $ Y.VarE (yicesname (yicescon n))
yExp _ (VarE (Sig n _)) = return $ Y.VarE (yicesname n)

-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
--
-- I don't have documentation for what yices allows in names, but it appears
-- symbols aren't allowed. So this just replaces each symbol with an ascii
-- approximation.
yicesname :: String -> String
yicesname [] = []
-- TODO: renaming of 'not' should be part of builtins, it should not go here.
yicesname "not" = "_not"
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
yicesname ('(':cs) = "__oparen" ++ yicesname cs
yicesname (')':cs) = "__cparen" ++ yicesname cs
yicesname (',':cs) = "__comma" ++ yicesname cs
yicesname (c:cs) = c : yicesname cs

-- Yices data constructors don't support partial application, so we wrap them in
-- functions given by the following name.
yicescon :: Name -> Name
yicescon n = "C" ++ n

yType :: Compiler -> Type -> YCM Y.TypY
yType _ (ConT n) = return $ Y.VarT (yicesname n)
yType c (AppT (AppT (ConT "->") a) b) = do
    a' <- compile_type c c a
    b' <- compile_type c c b
    return $ Y.ARR [a', b']
yType _ t = fail $ "yicesY does not apply to type: " ++ pretty t

coreY :: Compiler
coreY = Compiler [] yExp yType

yicesY :: Compiler
yicesY = compilers [preludeY, coreY]
            
-- compile_dec
--   Assumes the declaration is monomorphic.
compile_dec :: Compiler -> Dec -> [Y.CmdY]
compile_dec c (ValD (TopSig n [] t) e) =
    let yt = fromYCM $ compile_type c c t
        ye = fromYCM $ compile_exp c c e
    in [Y.DEFINE (yicesname n, yt) (Just ye)]
compile_dec c (DataD n [] cs) =
    let con :: Con -> (String, [(String, Y.TypY)])
        con (Con n ts) = (yicesname n, zip [yicesname n ++ show i | i <- [0..]]
                                 (map (fromYCM . compile_type c c) ts))

        -- Wrap each constructor in a function which supports partial
        -- application.
        mkcons :: Con -> Y.CmdY
        mkcons (Con cn ts) = 
            let yts = [fromYCM $ compile_type c c t | t <- ts]

                ft :: Y.TypY -> Y.TypY -> Y.TypY
                ft a b = Y.ARR [a, b]

                yt = foldr ft (Y.VarT (yicesname n)) yts

                fe :: (Name, Y.TypY) -> Y.ExpY -> Y.ExpY
                fe (n, t) e = Y.LAMBDA [(n, t)] e

                names = [[c] | c <- take (length ts) "abcdefghijklmnop"]

                body =  if null ts
                          then Y.VarE (yicesname cn)
                          else Y.APP (Y.VarE (yicesname cn)) (map Y.VarE names)
                ye = foldr fe body (zip names yts)
            in Y.DEFINE (yicesname (yicescon cn), yt) (Just ye)

        deftype = Y.DEFTYP (yicesname n) (Just (Y.DATATYPE (map con cs)))
        defcons = map mkcons cs
    in deftype : defcons
compile_dec c d
    = error $ "compile_dec: cannot compile to yices: " ++ pretty d

compile_decs :: Compiler -> [Dec] -> [Y.CmdY]
compile_decs c ds = concat $ map (compile_dec c) ds

