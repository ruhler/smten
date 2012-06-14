
module Seri.Target.Elaborate.Integer (integerR) where

import Seri.Lambda
import Seri.Target.Elaborate.Elaborate

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

