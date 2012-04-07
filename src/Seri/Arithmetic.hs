
module Seri.Arithmetic (
    addP, subP, mulP, ltP,
    arithR
 ) where

import Seri.Elaborate
import Seri.IR
import Seri.Typed

addP :: Typed Exp (Integer -> Integer -> Integer)
addP = primitive "+"

subP :: Typed Exp (Integer -> Integer -> Integer)
subP = primitive "-"

mulP :: Typed Exp (Integer -> Integer -> Integer)
mulP = primitive "*"

ltP :: Typed Exp (Integer -> Integer -> Bool)
ltP = primitive "<"

arithR :: Rule
arithR = Rule $ \decls gr e ->
    case e of 
      (AppE _ (AppE _ (PrimE _ "+") (IntegerE a)) (IntegerE b))
        -> Just $ IntegerE (a+b)
      (AppE _ (AppE _ (PrimE _ "-") (IntegerE a)) (IntegerE b))
        -> Just $ IntegerE (a-b)
      (AppE _ (AppE _ (PrimE _ "*") (IntegerE a)) (IntegerE b))
        -> Just $ IntegerE (a*b)
      (AppE _ (AppE _ (PrimE _ "<") (IntegerE a)) (IntegerE b))
        -> Just $ if a < b then trueE else falseE
      _ -> Nothing

