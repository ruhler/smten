
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors working with Exp
module Seri.Exp.Sugar (
    litE, conE, de_conE, varE, de_varE, appE, de_appE, appsE, de_appsE, lamE,
    lamsE, letE, de_letE, ifE, typeE, caseE, de_litE,
    
    boolE, de_boolE, falseE, trueE, charE, de_charE, listE, de_listE, stringE, de_stringE,
    errorE, tupleE,
    integerE, de_integerE, numberE,

    addE, subE, mulE, opE,
    de_notE, de_andE, de_orE,
    eqE, ltE, leqE, gtE, 
    ) where

import Control.Monad

import Seri.Lit
import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Exp.Exp
import Seri.Exp.Typeof

conE :: Sig -> Exp
conE = ConE

de_conE :: Exp -> Maybe Sig
de_conE (ConE s) = Just s
de_conE _ = Nothing

varE :: Sig -> Exp
varE = VarE

de_varE :: Exp -> Maybe Sig
de_varE (VarE s) = Just s
de_varE _ = Nothing

litE :: Lit -> Exp
litE = LitE

appE :: Exp -> Exp -> Exp
appE = AppE

appsE :: Exp -> [Exp] -> Exp
appsE = foldl AppE

lamE :: Sig -> Exp -> Exp
lamE = LamE

lamsE :: [Sig] -> Exp -> Exp
lamsE [] x = x
lamsE (v:vs) x = lamE v (lamsE vs x)

letE :: Sig -> Exp -> Exp -> Exp
letE s v b = appE (lamE s b) v

de_letE :: Exp -> Maybe (Sig, Exp, Exp)
de_letE (AppE (LamE s b) v) = Just (s, v, b)
de_letE _ = Nothing

de_appE :: Exp -> Maybe (Exp, Exp)
de_appE (AppE f x) = Just (f, x)
de_appE _ = Nothing

de_appsE :: Exp -> (Exp, [Exp])
de_appsE (AppE a b) =
    let (f, as) = de_appsE a
    in (f, as ++ [b])
de_appsE t = (t, [])

ifE :: Exp -> Exp -> Exp -> Exp
ifE p a b = CaseE p (Sig (name "True") boolT) a b

trueE :: Exp
trueE = conE (Sig (name "True") boolT)

falseE :: Exp
falseE = conE (Sig (name "False") boolT)

boolE :: Bool -> Exp
boolE True = trueE
boolE False = falseE

de_boolE :: Exp -> Maybe Bool
de_boolE e
  | e == trueE = Just True
  | e == falseE = Just False
  | otherwise = Nothing

charE :: Char -> Exp
charE = litE . charL

de_charE :: Exp -> Maybe Char
de_charE e = do
    l <- de_litE e
    de_charL l

de_litE :: Exp -> Maybe Lit
de_litE (LitE l) = Just l
de_litE _ = Nothing

-- | [a, b, ..., c]
listE :: [Exp] -> Exp
listE [] = conE (Sig (name "[]") (listT UnknownT))
listE [x] =
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE (conE (Sig (name ":") consT)) [x, conE (Sig (name "[]") (listT t))]
listE (x:xs) = 
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE (conE (Sig (name ":") consT)) [x, listE xs]

de_listE :: Exp -> Maybe [Exp]
de_listE e | Just (Sig n _) <- de_conE e, n == name "[]" = Just []
de_listE e | (f, [a, b]) <- de_appsE e = do
    (Sig n _) <- de_conE f
    guard $ n == name ":"
    xs <- de_listE b
    return (a:xs)
de_listE _ = Nothing

stringE :: String -> Exp
stringE str = listE (map charE str)

de_stringE :: Exp -> Maybe String
de_stringE e = do
    elems <- de_listE e
    guard $ not (null elems)
    mapM de_charE elems

-- Seri error, before flattening.
errorE :: String -> Exp
errorE msg = appE (varE (Sig (name "error") UnknownT)) (stringE msg)

-- | Type signature expression, of form: (e :: t)
-- Assigns the given type to the given expression.
--
-- For constructors and variables, we update the signatures directly.
-- For other expressions, we desugar to: ((id :: (t -> t)) e)
typeE :: Exp -> Type -> Exp
typeE (ConE (Sig n _)) t = ConE (Sig n t)
typeE (VarE (Sig n _)) t = VarE (Sig n t)
typeE e t = appE (varE (Sig (name "id") (arrowsT [t, t]))) e

tupleE :: [Exp] -> Exp
tupleE xs =
  let n = tupleN (length xs)
      ts = map typeof xs
      t = arrowsT $ ts ++ [tupleT ts]
  in appsE (conE (Sig n t)) xs

integerE :: Integer -> Exp
integerE = litE . integerL

de_integerE :: Exp -> Maybe Integer
de_integerE e = do
    l <- de_litE e
    de_integerL l

numberE :: Integer -> Exp
numberE i =
 let t = arrowsT [integerT, UnknownT]
 in appE (VarE (Sig (name "fromInteger") t)) (integerE i)

-- a + b
addE :: Exp -> Exp -> Exp
addE = opE "+"

-- a - b
subE :: Exp -> Exp -> Exp
subE = opE "-"

-- a * b
mulE :: Exp -> Exp -> Exp
mulE = opE "*"

opE :: String -> Exp -> Exp -> Exp
opE op a b = appsE (varE (Sig (name op) UnknownT)) [a, b]

de_notE :: Exp -> Maybe Exp
de_notE e =
  case de_appsE e of
    (VarE (Sig n _), [x]) | n == name "Prelude.not" -> Just x
    _ -> Nothing

de_andE :: Exp -> Maybe (Exp, Exp)
de_andE e =
  case de_appsE e of
    (VarE (Sig n _), [x, y]) | n == name "Prelude.and" -> Just (x, y)
    _ -> Nothing

de_orE :: Exp -> Maybe (Exp, Exp)
de_orE e =
  case de_appsE e of
    (VarE (Sig n _), [x, y]) | n == name "Prelude.or" -> Just (x, y)
    _ -> Nothing

caseE :: Exp -> Sig -> Exp -> Exp -> Exp
caseE x (Sig kn _) y n
  | (ConE (Sig s _), vs) <- de_appsE x =
    if kn == s
        then appsE y vs
        else n
caseE x k y n = CaseE x k y n

eqE :: Exp -> Exp -> Exp
eqE a b = appsE (varE (Sig (name "Prelude.==") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

ltE :: Exp -> Exp -> Exp
ltE a b = appsE (varE (Sig (name "Prelude.<") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

leqE :: Exp -> Exp -> Exp
leqE a b = appsE (varE (Sig (name "Prelude.<=") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

gtE :: Exp -> Exp -> Exp
gtE a b = appsE (varE (Sig (name "Prelude.>") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

