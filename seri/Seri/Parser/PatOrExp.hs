
module Seri.Parser.PatOrExp (
    PatOrExp, toPat, toExp,
    typePE, opPE, conopPE, appsPE,
    lamPE, letPE, ifPE, casePE, doPE,
    varPE, conPE, integerPE, charPE, stringPE, 
    tuplePE, fromtoPE, lcompPE, listPE, updatePE,
 ) where

import Data.Functor ((<$>))

import Seri.Failable
import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Exp
import Seri.Dec

import Seri.Parser.Monad
import Seri.Parser.Utils

type PatOrExp = (Failable Pat, Failable Exp)

toPat :: PatOrExp -> ParserMonad Pat
toPat x = case attempt (fst x) of
             Left msg -> failE msg
             Right v -> return v

toExp :: PatOrExp -> ParserMonad Exp
toExp x = case attempt (snd x) of
             Left msg -> failE msg
             Right v -> return v

typePE :: PatOrExp -> Type -> PatOrExp
typePE (_, e) t =
  let p' = throw "type signature not allowed in pattern"
      e' = flip typeE t <$> e
  in (p', e')

opPE :: String -> PatOrExp -> PatOrExp -> PatOrExp
opPE n (_, a) (_, b) =
  let p' = throw "operators now allowed in pattern"
      e' = do   
         [av, bv] <- sequence [a, b]
         return (opE n av bv)
  in (p', e')

conopPE :: String -> PatOrExp -> PatOrExp -> PatOrExp
conopPE n a b = appsPE (conPE (name n)) [a, b]

appsPE :: PatOrExp -> [PatOrExp] -> PatOrExp
appsPE (pf, ef) pexs =
 let p = do
        (f : xs) <- sequence (pf : map fst pexs)
        case f of
            ConP n [] -> return $ ConP n xs
            _ -> throw "unsupported application in pattern"
     e = do
        (f : xs) <- sequence (ef : map snd pexs)    
        return (appsE f xs)
 in (p, e)

lamPE :: Sig -> PatOrExp -> PatOrExp
lamPE s (_, e) = 
  let p = throw "lambda not allowed in pattern"
      e' = lamE s <$> e 
  in (p, e')

letPE :: [LDec] -> PatOrExp -> PatOrExp
letPE decls (_, e) =
  let p = throw "let not allowed in pattern"
      e' = mletsE (lcoalesce decls) <$> e
  in (p, e')

ifPE :: PatOrExp -> PatOrExp -> PatOrExp -> PatOrExp
ifPE (_, a) (_, b) (_, c) =
  let p = throw "if not allowed in pattern"
      e' = do
        [av, bv, cv] <- sequence [a, b, c]
        return $ ifE av bv cv
  in (p, e')

casePE :: PatOrExp -> [SMatch] -> PatOrExp
casePE (_, e) ms =
  let p = throw "case not allowed in pattern"
      e' = flip mcaseE ms <$> e
  in (p, e')

doPE :: [Stmt] -> PatOrExp
doPE stmts =
  let p = throw "do not allowed in patern"
      e = return $ doE stmts
  in (p, e)

varPE :: Name -> PatOrExp
varPE n = 
  let p = return $ if n == name "_" then WildP else VarP n
      e = return $ VarE (Sig n UnknownT)
  in (p, e)

conPE :: Name -> PatOrExp
conPE n =
  let p = return $ ConP n []
      e = return $ ConE (Sig n UnknownT)
  in (p, e)

integerPE :: Integer -> PatOrExp
integerPE i =
  let p = return $ numberP i
      e = return $ numberE i
  in (p, e)

charPE :: Char -> PatOrExp
charPE c =
  let p = return $ charP c
      e = return $ charE c
  in (p, e)

stringPE :: String -> PatOrExp
stringPE s =
  let p = return $ stringP s
      e = return $ stringE s
  in (p, e)

tuplePE :: [PatOrExp] -> PatOrExp
tuplePE xs =
  let p = tupleP <$> sequence (map fst xs)
      e = tupleE <$> sequence (map snd xs)
  in (p, e)

listPE :: [PatOrExp] -> PatOrExp
listPE xs =
  let p = listP <$> sequence (map fst xs)
      e = listE <$> sequence (map snd xs)
  in (p, e)

fromtoPE :: PatOrExp -> PatOrExp -> PatOrExp
fromtoPE (_, a) (_, b) =
  let p = throw "fromto not allowed in pattern"
      e = do
        [av, bv] <- sequence [a, b]
        return (fromtoE av bv)
  in (p, e)

lcompPE :: PatOrExp -> [Qual] -> PatOrExp
lcompPE (_, a) qs =
  let p = throw "list comprehension not allowed in pattern"
      e = do
          av <- a
          return (lcompE av qs)
  in (p, e)

updatePE :: PatOrExp -> [(Name, Exp)] -> PatOrExp
updatePE (_, a) bs =
  let p = throw "labelled patterns not yet supported"
      e = do
          av <- a 
          return $ 
            case av of
               ConE s -> recordC s bs
               _ -> recordU av bs
  in (p, e)

