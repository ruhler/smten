
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Data.Char0 (char_eq) where

import qualified Prelude as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Bool
import Smten.Runtime.SmtenHS

char_eq :: Char -> Char -> Bool
char_eq a@(C# {}) b@(C# {}) = if toHSChar a P.== toHSChar b then True else False
char_eq (Char_Err msg) _ = error0 msg
char_eq _ (Char_Err msg) = error0 msg
char_eq (Char_Prim r a) b = primitive0 (\m -> char_eq (r m) (realize m b)) (char_eq a b)
char_eq a (Char_Prim r b) = primitive0 (\m -> char_eq (realize m a) (r m)) (char_eq a b)
char_eq (Char_Ite p x y) b = ite0 p (char_eq x b) (char_eq y b)
char_eq a (Char_Ite p x y) = ite0 p (char_eq a x) (char_eq a y)

