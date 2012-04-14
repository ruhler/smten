
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Integer where

import Seri
import Seri.Lib.Bool

instance SeriType Integer where
    seritype _ = ConT "Integer"

declprim "+" [t| Integer -> Integer -> Integer |]
declprim "-" [t| Integer -> Integer -> Integer |]
declprim "*" [t| Integer -> Integer -> Integer |]
declprim "<" [t| Integer -> Integer -> Bool |]
declprim ">" [t| Integer -> Integer -> Bool |]

arithR :: Rule
arithR = Rule $ \gr e ->
    case val e of 
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

