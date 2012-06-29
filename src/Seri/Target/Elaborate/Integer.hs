
-- | Rules for reducing integer primitives.
module Seri.Target.Elaborate.Integer (integerR) where

import Seri.Lambda
import Seri.Target.Elaborate.Elaborate

integerR :: (Monad m) => Rule m
integerR = Rule $ \gr env e ->
    case e of 
      (AppE (AppE (VarE (Sig "__prim_add_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a+b)
      (AppE (AppE (VarE (Sig "__prim_sub_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a-b)
      (AppE (AppE (VarE (Sig "__prim_mul_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ IntegerE (a*b)
      (AppE (AppE (VarE (Sig "<" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a < b then trueE else falseE
      (AppE (AppE (VarE (Sig ">" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a > b then trueE else falseE
      (AppE (AppE (VarE (Sig "__prim_eq_Integer" _)) (IntegerE a)) (IntegerE b))
        -> return . Just $ if a == b then trueE else falseE
      _ -> return Nothing

