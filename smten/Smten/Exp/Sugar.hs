
{-# LANGUAGE PatternGuards #-}

-- | Abstract constructors and deconstructors working with Exp
module Smten.Exp.Sugar (
    litE, conE, de_conE, varE, de_varE,
    appE, de_appE, appsE, de_appsE,
    lamE, lamsE, de_lamE, de_lamsE,
    letE, de_letE, letsE, ifE, sigE, caseE, de_litE,

    boolE, de_boolE, falseE, trueE, charE, de_charE,
    listE, de_listE, stringE, de_stringE,
    de_bitE,
    errorE, tupleE, de_tupleE,
    integerE, de_integerE, numberE,

    addE, subE, mulE, opE,
    eqE, ltE, leqE, gtE, 
    fromtoE, fromthentoE, fromE, fromthenE,
    ) where

import Control.Monad
import Data.List(genericLength)

import Smten.Location
import Smten.Bit
import Smten.Lit
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Exp.Exp
import Smten.Exp.Typeof

conE :: Location -> Sig -> Exp
conE = ConE

de_conE :: Exp -> Maybe Sig
de_conE (ConE _ s) = Just s
de_conE _ = Nothing

varE :: Location -> Sig -> Exp
varE = VarE

de_varE :: Exp -> Maybe Sig
de_varE (VarE _ s) = Just s
de_varE _ = Nothing

litE :: Location -> Lit -> Exp
litE = LitE

appE :: Location -> Exp -> Exp -> Exp
appE = AppE

appsE :: Location -> Exp -> [Exp] -> Exp
appsE l = foldl (AppE l)

lamE :: Location -> Sig -> Exp -> Exp
lamE = LamE

lamsE :: Location -> [Sig] -> Exp -> Exp
lamsE _ [] x = x
lamsE l (v:vs) x = lamE l v (lamsE l vs x)

de_lamE :: Exp -> Maybe (Sig, Exp)
de_lamE (LamE _ s e) = Just (s, e)
de_lamE _ = Nothing

de_lamsE :: Exp -> ([Sig], Exp)
de_lamsE e
 | Just (s, b) <- de_lamE e
   = let (s', b') = de_lamsE b
     in (s:s', b')
 | otherwise = ([], e)

letE :: Location -> Sig -> Exp -> Exp -> Exp
letE l s v b = appE l (lamE l s b) v

letsE :: Location -> [(Sig, Exp)] -> Exp -> Exp
letsE _ [] x = x
letsE l ((s, v):bs) x = letE l s v (letsE l bs x)

de_letE :: Exp -> Maybe (Sig, Exp, Exp)
de_letE (AppE _ (LamE _ s b) v) = Just (s, v, b)
de_letE _ = Nothing

de_appE :: Exp -> Maybe (Exp, Exp)
de_appE (AppE _ f x) = Just (f, x)
de_appE _ = Nothing

de_appsE :: Exp -> (Exp, [Exp])
de_appsE (AppE _ a b) =
    let (f, as) = de_appsE a
    in (f, as ++ [b])
de_appsE t = (t, [])

ifE :: Location -> Exp -> Exp -> Exp -> Exp
ifE l p a b = CaseE l p (Sig trueN boolT) a b

trueE :: Location -> Exp
trueE l = conE l (Sig trueN boolT)

falseE :: Location -> Exp
falseE l = conE l (Sig falseN boolT)

boolE :: Location -> Bool -> Exp
boolE l True = trueE l
boolE l False = falseE l

de_boolE :: Exp -> Maybe Bool
de_boolE e = do
    Sig nm _ <- de_conE e
    guard $ nm `elem` [trueN, falseN]
    return (nm == trueN)

charE :: Location -> Char -> Exp
charE l = litE l . charL

de_charE :: Exp -> Maybe Char
de_charE e = do
    l <- de_litE e
    de_charL l

de_litE :: Exp -> Maybe Lit
de_litE (LitE _ l) = Just l
de_litE _ = Nothing

-- | [a, b, ..., c]
listE :: Location -> [Exp] -> Exp
listE l [] = conE l (Sig (name "[]") (listT UnknownT))
listE l [x] =
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE l (conE l (Sig (name ":") consT)) [x, conE l (Sig (name "[]") (listT t))]
listE l (x:xs) = 
 let t = typeof x
     consT = arrowsT [t, listT t, listT t]
 in appsE l (conE l (Sig (name ":") consT)) [x, listE l xs]

de_listE :: Exp -> Maybe [Exp]
de_listE e | Just (Sig n _) <- de_conE e, n == name "[]" = Just []
de_listE e | (f, [a, b]) <- de_appsE e = do
    (Sig n _) <- de_conE f
    guard $ n == name ":"
    xs <- de_listE b
    return (a:xs)
de_listE _ = Nothing

stringE :: Location -> String -> Exp
stringE l str = listE l (map (charE l) str)

de_stringE :: Exp -> Maybe String
de_stringE e = do
    elems <- de_listE e
    guard $ not (null elems)
    mapM de_charE elems

-- Smten error, before flattening.
errorE :: Location -> String -> Exp
errorE l msg = appE l (varE l (Sig (name "error") UnknownT)) (stringE l msg)

-- | Type signature expression, of form: (e :: t)
-- Assigns the given type to the given expression.
--
-- For constructors and variables, we update the signatures directly.
-- For other expressions, we desugar to: ((id :: (t -> t)) e)
sigE :: Location -> Exp -> Type -> Exp
sigE _ (ConE l (Sig n _)) t = ConE l (Sig n t)
sigE _ (VarE l (Sig n _)) t = VarE l (Sig n t)
sigE l e t = appE l (varE l (Sig (name "id") (arrowsT [t, t]))) e

tupleE :: Location -> [Exp] -> Exp
tupleE l xs =
  let n = tupleN (length xs)
      ts = map typeof xs
      t = arrowsT $ ts ++ [tupleT ts]
  in appsE l (conE l (Sig n t)) xs

de_tupleE :: Exp -> Maybe [Exp]
de_tupleE x = 
    case de_appsE x of
       (ConE _ (Sig nm _), xs) -> do
          n <- de_tupleN nm
          guard $ genericLength xs == n
          return xs
       _ -> Nothing

integerE :: Location -> Integer -> Exp
integerE l = litE l . integerL

de_integerE :: Exp -> Maybe Integer
de_integerE e = do
    l <- de_litE e
    de_integerL l

de_bitE :: Exp -> Maybe Bit
de_bitE e = do
    l <- de_litE e
    de_bitL l

numberE :: Location -> Integer -> Exp
numberE l i =
 let t = arrowsT [integerT, UnknownT]
 in appE l (VarE l (Sig (name "fromInteger") t)) (integerE l i)

-- a + b
addE :: Location -> Exp -> Exp -> Exp
addE = opE "+"

-- a - b
subE :: Location -> Exp -> Exp -> Exp
subE = opE "-"

-- a * b
mulE :: Location -> Exp -> Exp -> Exp
mulE = opE "*"

opE :: String -> Location -> Exp -> Exp -> Exp
opE op l a b = appsE l (varE l (Sig (name op) UnknownT)) [a, b]

caseE :: Location -> Exp -> Sig -> Exp -> Exp -> Exp
caseE l x (Sig kn _) y n
  | (ConE _ (Sig s _), vs) <- de_appsE x =
    if kn == s
        then appsE l y vs
        else n
caseE l x k y n = CaseE l x k y n

eqE :: Location -> Exp -> Exp -> Exp
eqE l a b = appsE l (varE l (Sig (name "Prelude.==") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

ltE :: Location -> Exp -> Exp -> Exp
ltE l a b = appsE l (varE l (Sig (name "Prelude.<") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

leqE :: Location -> Exp -> Exp -> Exp
leqE l a b = appsE l (varE l (Sig (name "Prelude.<=") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

gtE :: Location -> Exp -> Exp -> Exp
gtE l a b = appsE l (varE l (Sig (name "Prelude.>") (arrowsT [typeof a, typeof b, boolT]))) [a, b]

-- [a..b]
fromtoE :: Location -> Exp -> Exp -> Exp
fromtoE l a b = appsE l (varE l (Sig (name "enumFromTo") UnknownT)) [a, b]

-- [a,b,..]
fromthenE :: Location -> Exp -> Exp -> Exp
fromthenE l a b = appsE l (varE l (Sig (name "enumFromThen") UnknownT)) [a, b]

-- [a..]
fromE :: Location -> Exp -> Exp
fromE l a = appE l (varE l (Sig (name "enumFrom") UnknownT)) a

-- [a,b,..,c]
fromthentoE :: Location -> Exp -> Exp -> Exp -> Exp
fromthentoE l a b c = appsE l (varE l (Sig (name "enumFromThenTo") UnknownT)) [a, b, c]

