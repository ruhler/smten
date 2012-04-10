
{-# LANGUAGE TemplateHaskell #-}

module Seri.Integer where

import Seri.Bool
import Seri.Declarations
import Seri.Elaborate
import Seri.IR
import Seri.Typed

instance SeriType Integer where
    seritype _ = ConT "Integer"

declprim "+" [t| Typed Exp (Integer -> Integer -> Integer) |]
declprim "-" [t| Typed Exp (Integer -> Integer -> Integer) |]
declprim "*" [t| Typed Exp (Integer -> Integer -> Integer) |]
declprim "<" [t| Typed Exp (Integer -> Integer -> Bool) |]
declprim ">" [t| Typed Exp (Integer -> Integer -> Bool) |]

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
      (AppE _ (AppE _ (PrimE _ ">") (IntegerE a)) (IntegerE b))
        -> Just $ if a > b then trueE else falseE
      _ -> Nothing

