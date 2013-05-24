{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

import Prelude hiding (foldr, replicate, (&&), and, Num(..), Eq(..), Bool(..), Integer)
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude

class HaskellF a => Num a
    where (-) :: Function a (Function a a)

instance Num Integer
    where (-) = __prim_sub_Integer

class HaskellF a => Eq a
    where (==) :: Function a (Function a Bool)

instance Eq Integer
    where (==) = __prim_eq_Integer

foldr :: (HaskellF a, HaskellF b) => Function (Function a (Function b b)) (Function b (Function (List__ a) b))
foldr = lamHF "f" $ \f -> lamHF "z" $ \z -> lamHF "l" $ \l ->
    __caseCons__ l
       (lamHF "x" (\x -> lamHF "xs" (\xs ->
          applyHF (applyHF f x) (applyHF (applyHF (applyHF foldr f) z) xs))))
       z

replicate ::  HaskellF a => Function Integer (Function a (List__ a))
replicate = lamHF "n" (\n -> lamHF "x" (\x ->
  let _s2 = applyHF (applyHF __mkCons__ x) (applyHF (applyHF replicate (applyHF (applyHF (-) n) 1)) x)
  in __caseTrue (applyHF (applyHF (==) 0) n) __mkNil__ _s2))

and :: Function (List__ Bool) Bool
and = applyHF (applyHF foldr (&&)) __mkTrue

(&&) :: Function Bool (Function Bool Bool)
(&&) = lamHF "a" (\a -> lamHF "b" (\b -> __caseTrue a b __mkFalse))

result :: Bool
result = applyHF and elems

elems = applyHF (applyHF replicate 400000) __mkTrue

main :: Prelude.IO ()
main = putStrLn $ show (unbox result) 

