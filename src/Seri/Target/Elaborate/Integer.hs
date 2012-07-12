
-- | Rules for reducing integer primitives.
module Seri.Target.Elaborate.Integer (integerR) where

import Seri.Lambda
import Seri.Target.Elaborate.Elaborate

isibop :: String -> Exp -> Bool
isibop op (AppE (AppE (VarE (Sig nm _)) (LitE (IntegerL _))) (LitE (IntegerL _))) = nm == op
isibop _ _ = False

apibop :: (Integer -> Integer -> Exp) -> Exp -> Exp 
apibop f (AppE (AppE (VarE (Sig _ _)) (LitE (IntegerL a))) (LitE (IntegerL b))) = f a b
apibop f e = error $ "not an integer binary operation: " ++ pretty e

integerR :: (Monad m) => Rule m
integerR = Rule $ \gr env e ->
    case e of 
        _ | isibop "__prim_add_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a+b)) e
        _ | isibop "__prim_sub_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a-b)) e
        _ | isibop "__prim_mul_Integer" e ->
            return . Just $ apibop (\a b -> integerE (a*b)) e
        _ | isibop "<" e ->
            return . Just $ apibop (\a b -> boolE (a < b)) e
        _ | isibop ">" e ->
            return . Just $ apibop (\a b -> boolE (a > b)) e
        _ | isibop "__prim_eq_Integer" e ->
            return . Just $ apibop (\a b -> boolE (a == b)) e
        _ -> return Nothing

