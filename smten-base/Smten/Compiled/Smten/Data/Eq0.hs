
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Eq0 (int_eq, integer_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool
import Smten.Runtime.SmtenHS

int_eq :: Int -> Int -> Bool
int_eq a b = if a P.== b then True else False

integer_eq :: Integer -> Integer -> Bool
integer_eq (Integer a) (Integer b) = if a P.== b then True else False
integer_eq (Integer_Err msg) _ = error0 msg
integer_eq _ (Integer_Err msg) = error0 msg
integer_eq (Integer_Prim r a) b = primitive0 (\m -> integer_eq (r m) (realize m b)) (integer_eq a b)
integer_eq a (Integer_Prim r b) = primitive0 (\m -> integer_eq (realize m a) (r m)) (integer_eq a b)
integer_eq (Integer_Ite p x y) b = ite0 p (integer_eq x b) (integer_eq y b)
integer_eq a (Integer_Ite p x y) = ite0 p (integer_eq a x) (integer_eq a y)

