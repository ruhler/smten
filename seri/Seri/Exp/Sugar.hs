
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors working with Exp
module Seri.Exp.Sugar (
    litE, conE, de_conE, varE, de_varE, appE, de_appE, appsE, de_appsE, lamE,
    lamsE, de_letE, ifE, typeE,
    
    boolE, falseE, trueE, charE, de_charE, listE, de_listE, stringE, de_stringE,
    errorE, tupleE,
    integerE, numberE,
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

charE :: Char -> Exp
charE = litE . CharL

de_charE :: Exp -> Maybe Char
de_charE (LitE (CharL c)) = Just c
de_charE _ = Nothing

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
integerE = litE . IntegerL

numberE :: Integer -> Exp
numberE i =
 let t = arrowsT [integerT, UnknownT]
 in appE (VarE (Sig (name "fromInteger") t)) (integerE i)

