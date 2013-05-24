{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Prim

import Prelude hiding (foldr, replicate, (&&), and, Bool(..), Integer) 
import Smten.HaskellF.HaskellF

newtype List__ a = List____s ExpH

instance SmtenT1 List__ where
  smtenT1 _ = conT (name "Prelude.[]")

instance HaskellF1 List__ where
  box1 = List____s
  unbox1 x | List____s v <- x = v

__caseNil__ :: (HaskellF a, HaskellF z) => List__ a -> z -> z -> z
__caseNil__ = caseHF "Prelude.[]"

__caseCons__ :: (HaskellF a, HaskellF z) => List__ a -> Function a (Function (List__ a) z) -> z -> z
__caseCons__ = caseHF "Prelude.:"

__mkNil__ :: HaskellF a => List__ a
__mkNil__ = conHF' "Prelude.[]" []

__mkCons__ :: HaskellF a => Function a (Function (List__ a) (List__ a))
__mkCons__ = lamHF "x1" $ \x1 ->
                lamHF "x2" $ \x2 ->
                  conHF' "Prelude.:" [unbox x1, unbox x2]

newtype Bool = Bool__s ExpH

instance SmtenT Bool where
  smtenT _ = conT (name "Prelude.Bool")

instance HaskellF Bool where
  box = Bool__s
  unbox x | Bool__s v <- x = v

__caseTrue :: HaskellF z => Bool -> z -> z -> z
__caseTrue = caseHF "Prelude.True"

__caseFalse :: HaskellF z => Bool -> z -> z -> z
__caseFalse = caseHF "Prelude.False"

__mkTrue :: Bool
__mkTrue = conHF' "Prelude.True" []

__mkFalse :: Bool
__mkFalse = conHF' "Prelude.False" []

newtype Integer = Integer__s ExpH

instance SmtenT Integer where
    smtenT _ = integerT

instance HaskellF Integer where
    box = Integer__s
    unbox (Integer__s v) = v

instance Prelude.Num Integer where
    fromInteger = smtenHF
    (+) = error $ "+ for haskellf Integer not defined"
    (*) = error $ "* for haskellf Integer not defined"
    abs = error $ "abs for haskellf Integer not defined"
    signum = error $ "signum for haskellf Integer not defined"

__prim_sub_Integer :: Function Integer (Function Integer Integer)
__prim_sub_Integer = primHF sub_IntegerP

__prim_eq_Integer :: Function Integer (Function Integer Bool)
__prim_eq_Integer = primHF eq_IntegerP


foldr :: (HaskellF a, HaskellF b) => Function (Function a (Function b b)) (Function b (Function (List__ a) b))
foldr = lamHF "f" $ \f -> lamHF "z" $ \z -> lamHF "l" $ \l ->
    __caseCons__ l
       (lamHF "x" (\x -> lamHF "xs" (\xs ->
          applyHF (applyHF f x) (applyHF (applyHF (applyHF foldr f) z) xs))))
       z

replicate ::  HaskellF a => Function Integer (Function a (List__ a))
replicate = lamHF "n" (\n -> lamHF "x" (\x ->
  let _s2 = applyHF (applyHF __mkCons__ x) (applyHF (applyHF replicate (applyHF (applyHF __prim_sub_Integer n) 1)) x)
  in __caseTrue (applyHF (applyHF __prim_eq_Integer 0) n) __mkNil__ _s2))

and :: Function (List__ Bool) Bool
and = applyHF (applyHF foldr (&&)) __mkTrue

(&&) :: Function Bool (Function Bool Bool)
(&&) = lamHF "a" (\a -> lamHF "b" (\b -> __caseTrue a b __mkFalse))

result :: Bool
result = applyHF and elems

elems = applyHF (applyHF replicate 400000) __mkTrue

main :: Prelude.IO ()
main = putStrLn $ show (unbox result) 

