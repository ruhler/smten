
module Smten.Parser.PatOrExp (
    PatOrExp, toPat, toExp,
    sigPE, opPE, lopPE, conopPE, appsPE,
    lamPE, letPE, ifPE, casePE, doPE,
    varPE, conPE, integerPE, charPE, stringPE, 
    tuplePE, lcompPE, listPE, updatePE,
    fromtoPE, fromPE, fromthenPE, fromthentoPE,
    asPE,
 ) where

import Data.Functor ((<$>))

import Smten.Failable
import Smten.Location
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Exp
import Smten.Dec

import Smten.Parser.Monad
import Smten.Parser.Utils

type PatOrExp = (Failable Pat, Failable Exp)

toPat :: PatOrExp -> ParserMonad Pat
toPat x = case attempt (fst x) of
             Left msg -> lfailE msg
             Right v -> return v

toExp :: PatOrExp -> ParserMonad Exp
toExp x = case attempt (snd x) of
             Left msg -> lfailE msg
             Right v -> return v

sigPE :: Location -> PatOrExp -> Type -> PatOrExp
sigPE l (p, e) t =
  let p' = flip sigP t <$> p
      e' = flip (sigE l) t <$> e
  in (p', e')

lopPE :: String -> PatOrExp -> PatOrExp -> ParserMonad PatOrExp
lopPE s a b = withloc $ \l -> opPE s l a b

opPE :: String -> Location -> PatOrExp -> PatOrExp -> PatOrExp
opPE n l (_, a) (_, b) =
  let p' = throw "operators now allowed in pattern"
      e' = do   
         [av, bv] <- sequence [a, b]
         return (opE n l av bv)
  in (p', e')

conopPE :: Location -> String -> PatOrExp -> PatOrExp -> PatOrExp
conopPE l n a b = appsPE l (conPE l (name n)) [a, b]

appsPE :: Location -> PatOrExp -> [PatOrExp] -> PatOrExp
appsPE _ x [] = x
appsPE l (pf, ef) pexs =
 let p = do
        (f : xs) <- sequence (pf : map fst pexs)
        case f of
            ConP n [] -> return $ ConP n xs
            _ -> throw $ "unsupported application in pattern: " ++ show f ++ " " ++ show xs
     e = do
        (f : xs) <- sequence (ef : map snd pexs)    
        return (appsE l f xs)
 in (p, e)

lamPE :: Location -> [PatOrExp] -> PatOrExp -> PatOrExp
lamPE l ps (_, e) = 
  let p = throw "lambda not allowed in pattern"
      e' = do
        args <- mapM fst ps
        mlamE l args <$> e
  in (p, e')

letPE :: Location -> [LDec] -> PatOrExp -> PatOrExp
letPE l decls (_, e) =
  let p = throw "let not allowed in pattern"
      e' = mletsE l (lcoalesce decls) <$> e
  in (p, e')

ifPE :: Location -> PatOrExp -> PatOrExp -> PatOrExp -> PatOrExp
ifPE l (_, a) (_, b) (_, c) =
  let p = throw "if not allowed in pattern"
      e' = do
        [av, bv, cv] <- sequence [a, b, c]
        return $ ifE l av bv cv
  in (p, e')

casePE :: Location -> PatOrExp -> [Alt] -> PatOrExp
casePE l (_, e) ms =
  let p = throw "case not allowed in pattern"
      e' = flip (mcaseE l) ms <$> e
  in (p, e')

doPE :: Location -> [Stmt] -> PatOrExp
doPE l stmts =
  let p = throw "do not allowed in patern"
      e = return $ doE l stmts
  in (p, e)

varPE :: Location -> Name -> PatOrExp
varPE l n = 
  let p = return $ if n == name "_" then WildP else VarP n
      e = return $ VarE l (Sig n UnknownT)
  in (p, e)

conPE :: Location -> Name -> PatOrExp
conPE l n =
  let p = return $ ConP n []
      e = return $ ConE l (Sig n UnknownT)
  in (p, e)

integerPE :: Location -> Integer -> PatOrExp
integerPE l i =
  let p = return $ numberP l i
      e = return $ numberE l i
  in (p, e)

charPE :: Location -> Char -> PatOrExp
charPE l c =
  let p = return $ charP l c
      e = return $ charE l c
  in (p, e)

stringPE :: Location -> String -> PatOrExp
stringPE l s =
  let p = return $ stringP l s
      e = return $ stringE l s
  in (p, e)

tuplePE :: Location -> [PatOrExp] -> PatOrExp
tuplePE l xs =
  let p = tupleP <$> sequence (map fst xs)
      e = tupleE l <$> sequence (map snd xs)
  in (p, e)

listPE :: Location -> [PatOrExp] -> PatOrExp
listPE l xs =
  let p = listP <$> sequence (map fst xs)
      e = listE l <$> sequence (map snd xs)
  in (p, e)

fromPE :: Location -> PatOrExp -> PatOrExp
fromPE l (_, a) =
  let p = throw "from not allowed in pattern"
      e = fromE l <$> a
  in (p, e)

fromtoPE :: Location -> PatOrExp -> PatOrExp -> PatOrExp
fromtoPE l (_, a) (_, b) =
  let p = throw "fromto not allowed in pattern"
      e = do
        [av, bv] <- sequence [a, b]
        return (fromtoE l av bv)
  in (p, e)

fromthenPE :: Location -> PatOrExp -> PatOrExp -> PatOrExp
fromthenPE l (_, a) (_, b) =
  let p = throw "fromthen not allowed in pattern"
      e = do
        [av, bv] <- sequence [a, b]
        return (fromthenE l av bv)
  in (p, e)

fromthentoPE :: Location -> PatOrExp -> PatOrExp -> PatOrExp -> PatOrExp
fromthentoPE l (_, a) (_, b) (_, c)=
  let p = throw "fromthento not allowed in pattern"
      e = do
        [av, bv, cv] <- sequence [a, b, c]
        return (fromthentoE l av bv cv)
  in (p, e)

lcompPE :: Location -> PatOrExp -> [Guard] -> PatOrExp
lcompPE l (_, a) qs =
  let p = throw "list comprehension not allowed in pattern"
      e = do
          av <- a
          return (lcompE l av qs)
  in (p, e)

updatePE :: Location -> PatOrExp -> [(Name, Exp)] -> PatOrExp
updatePE l (_, a) bs =
  let p = throw "labelled patterns not yet supported"
      e = do
          av <- a 
          return $ 
            case av of
               ConE _ s -> recordC l s bs
               _ -> recordU l av bs
  in (p, e)

asPE :: Name -> PatOrExp -> PatOrExp
asPE n (a, _) = 
  let p = AsP n <$> a
      e = throw "as patterns not allowed in expressions"
  in (p, e)

