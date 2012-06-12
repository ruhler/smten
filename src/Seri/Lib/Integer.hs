
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Integer where

import Seri
import Seri.Lib.Bool

declclass ''Num

decltycon 0 ''Integer

declprim "__prim_add_Integer" [t| Integer -> Integer -> Integer |]
declprim "__prim_sub_Integer" [t| Integer -> Integer -> Integer |]
declprim "__prim_mul_Integer" [t| Integer -> Integer -> Integer |]
declprim "<" [t| Integer -> Integer -> Bool |]
declprim ">" [t| Integer -> Integer -> Bool |]
declprim "==" [t| Integer -> Integer -> Bool |]

[s| 
    instance Num Integer where
        (+) = __prim_add_Integer
        (-) = __prim_sub_Integer
        (*) = __prim_mul_Integer
|]

integerR :: (Monad m) => Rule m
integerR = Rule $ \gr e ->
    case val e of 
      (AppE (AppE (PrimE (Sig "__prim_add_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a+b)
      (AppE (AppE (PrimE (Sig "__prim_sub_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a-b)
      (AppE (AppE (PrimE (Sig "__prim_mul_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a*b)
      (AppE (AppE (PrimE (Sig "<" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a < b then trueE else falseE
      (AppE (AppE (PrimE (Sig ">" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a > b then trueE else falseE
      (AppE (AppE (PrimE (Sig "==" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a == b then trueE else falseE
      _ -> return Nothing

