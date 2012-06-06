
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Integer where

import Seri
import Seri.Lib.Bool

decltycon 0 ''Integer

declprim "+" [t| Integer -> Integer -> Integer |]
declprim "-" [t| Integer -> Integer -> Integer |]
declprim "*" [t| Integer -> Integer -> Integer |]
declprim "<" [t| Integer -> Integer -> Bool |]
declprim ">" [t| Integer -> Integer -> Bool |]
declprim "==" [t| Integer -> Integer -> Bool |]

integerR :: (Monad m) => Rule m
integerR = Rule $ \gr e ->
    case val e of 
      (AppE (AppE (PrimE (Sig "+" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a+b)
      (AppE (AppE (PrimE (Sig "-" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a-b)
      (AppE (AppE (PrimE (Sig "*" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a*b)
      (AppE (AppE (PrimE (Sig "<" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a < b then trueE else falseE
      (AppE (AppE (PrimE (Sig ">" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a > b then trueE else falseE
      (AppE (AppE (PrimE (Sig "==" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a == b then trueE else falseE
      _ -> return Nothing

