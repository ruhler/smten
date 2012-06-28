
module Seri.Target.Yices.Yices (yicesN, yicesE, yicesT, yicesD) where

import qualified Math.SMT.Yices.Syntax as Y

import Seri.Failable
import Seri.Lambda

-- | Convert a seri name to a yices name.
yicesN :: String -> String
yicesN = yicesname

-- | Compile a seri expression to a yices expression.
-- The expression should be monomorphic.
yicesE :: Exp -> Failable Y.ExpY
yicesE = yExp

-- | Compile a seri type to a yices type
-- The type should be monomorphic.
yicesT :: Type -> Failable Y.TypY
yicesT = yType

-- | Compile a seri declarations to yices declarations.
-- The declarations should be monomorphic and in dependency order.
yicesD :: Dec -> Failable [Y.CmdY]
yicesD = yDec

-- Translate a seri expression to a yices expression
yExp :: Exp -> Failable Y.ExpY
yExp (IntegerE x) = return $ Y.LitI x
yExp e@(CaseE _ []) = fail $ "empty case statement: " ++ pretty e
yExp (CaseE e ms) =
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
      depat (VarP (Sig n t)) e = ([], [((n, attemptM $ yType t), e)])
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
      dematch :: Y.ExpY -> [Match] -> Failable Y.ExpY
      dematch e [Match p b] = do
          -- TODO: don't assume the last alternative will always be taken.
          let ConT dn = typeof b
          b' <- yExp b
          let (_, bindings) = depat p e
          return $ Y.LET bindings b'
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- yExp b
          let (preds, bindings) = depat p e
          let pred = yand preds
          return $ Y.IF pred (Y.LET bindings b') bms
  in do
      e' <- yExp e
      dematch e' ms
yExp (AppE a b) = do
    a' <- yExp a
    b' <- yExp b
    return $ Y.APP a' [b']
yExp (LamE (Sig n t) e) = do
    e' <- yExp e
    t' <- yType t
    return $ Y.LAMBDA [(n, t')] e'
yExp (ConE (Sig n _)) = return $ Y.VarE (yicescon n)
yExp (VarE (Sig n _)) = return $ Y.VarE (yicesname n)

-- Yices data constructors don't support partial application, so we wrap them in
-- functions given by the following name.
yicescon :: Name -> Name
yicescon n = yicesname $ "C" ++ n

yType :: Type -> Failable Y.TypY
yType (ConT n) = return $ Y.VarT (yicesname n)
yType (AppT (AppT (ConT "->") a) b) = do
    a' <- yType a
    b' <- yType b
    return $ Y.ARR [a', b']
yType t = fail $ "Cannot compile to yices: " ++ pretty t

-- yDec
--   Assumes the declaration is monomorphic.
yDec :: Dec -> Failable [Y.CmdY]
yDec (ValD (TopSig n [] t) e) = do
    yt <- yType t
    ye <- yExp e
    return [Y.DEFINE (yicesname n, yt) (Just ye)]
yDec (DataD "Integer" _ _) =
    let deftype = Y.DEFTYP "Integer" (Just (Y.VarT "int"))
    in return [deftype]
yDec (DataD n [] cs) =
    let con :: Con -> Failable (String, [(String, Y.TypY)])
        con (Con n ts) = do 
            ts' <- mapM yType ts
            return (yicesname n, zip [yicesname n ++ show i | i <- [0..]] ts')

        -- Wrap each constructor in a function which supports partial
        -- application.
        mkcons :: Con -> Failable Y.CmdY
        mkcons (Con cn ts) = do
            yts <- mapM yType ts
            let ft a b = Y.ARR [a, b]
            let yt = foldr ft (Y.VarT (yicesname n)) yts
            let fe (n, t) e = Y.LAMBDA [(n, t)] e
            let names = [[c] | c <- take (length ts) "abcdefghijklmnop"]
            let body =  if null ts
                          then Y.VarE (yicesname cn)
                          else Y.APP (Y.VarE (yicesname cn)) (map Y.VarE names)
            let ye = foldr fe body (zip names yts)
            return $ Y.DEFINE (yicescon cn, yt) (Just ye)
    in do
        cs' <- mapM con cs
        let deftype = Y.DEFTYP (yicesname n) (Just (Y.DATATYPE cs'))
        defcons <- mapM mkcons cs
        return $ deftype : defcons

-- Integer Primitives
yDec (PrimD (TopSig "__prim_add_Integer" _ _))
 = return [defiop "__prim_add_Integer" "+"]
yDec (PrimD (TopSig "__prim_sub_Integer" _ _))
 = return [defbop "__prim_sub_Integer" "-"]
yDec (PrimD (TopSig "<" _ _)) = return [defbop "<" "<"]
yDec (PrimD (TopSig ">" _ _)) = return [defbop ">" ">"]
yDec (PrimD (TopSig "__prim_eq_Integer" _ _))
 = return [defbop "__prim_eq_Integer" "="]

yDec d = fail $ "Cannot compile to yices: " ++ pretty d

-- defiop name type op
--   Define a primitive binary integer operation.
--   name - the name of the primitive
--   op - the integer operation.
defiop :: String -> String -> Y.CmdY
defiop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Integer))")
        (Just (Y.VarE $
            "(lambda (a::Integer) (lambda (b::Integer) (" ++ op ++ " a b)))"))

-- defbop name type op
--   Define a primitive binary integer predicate.
--   name - the name of the primitive
--   op - the predicate operator.
defbop :: String -> String -> Y.CmdY
defbop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Bool))")
        (Just (Y.VarE $ unlines [
                "(lambda (a::Integer) (lambda (b::Integer)",
                " (if (" ++ op ++ " a b) True False)))"]))


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

