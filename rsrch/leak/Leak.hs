
{-# LANGUAGE FlexibleInstances #-}

import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH
import Smten.Prim

import Prelude hiding (foldr, replicate, (&&), and) 

newtype ExpHF a = ExpHF {
    unbox :: ExpH
}

applyHF :: ExpHF (a -> b) -> ExpHF a -> ExpHF b
applyHF f x = ExpHF (appEH (unbox f) (unbox x))

lamHF :: String -> (ExpHF a -> ExpHF b) -> ExpHF (a->b)
lamHF _ f =
  let g :: ExpH -> ExpH
      g x = unbox (f (ExpHF x))
      
      s = error "lamHF sig"
      tb = error "lamHF tb"
  in ExpHF (lamEH s tb g)

conHF0 :: String -> ExpHF a
conHF0 nm = ExpHF (aconEH (name nm) (error "conHF' t") [])

conHF2 :: String -> ExpHF a -> ExpHF b -> ExpHF c
conHF2 nm a b = ExpHF (aconEH (name nm) (error "conHF' t") [unbox a, unbox b])

caseHF0 :: String -> ExpHF a -> ExpHF b -> ExpHF b -> ExpHF b
caseHF0 k x y n = ExpHF (caseEH (error "tcs") (unbox x) (Sig (name k) (error "caseHF t")) (unbox y) (unbox n))

caseHF2 :: String -> ExpHF a -> ExpHF (b -> c -> d) -> ExpHF d -> ExpHF d
caseHF2 k x y n = ExpHF (caseEH (error "tcs") (unbox x) (Sig (name k) (error "caseHF t")) (unbox y) (unbox n))

__caseNil__ :: ExpHF [a] -> ExpHF b -> ExpHF b -> ExpHF b
__caseNil__ = caseHF0 "Prelude.[]"

__caseCons__ :: ExpHF [a] -> ExpHF (a -> [a] -> b) -> ExpHF b -> ExpHF b
__caseCons__ = caseHF2 "Prelude.:"

__mkNil__ :: ExpHF [a]
__mkNil__ = conHF0 "Prelude.[]"

__mkCons__ :: ExpHF (a -> [a] -> [a])
__mkCons__ = lamHF "x1" $ \x1 ->
                lamHF "x2" $ \x2 ->
                  conHF2 "Prelude.:" x1 x2

__caseTrue :: ExpHF Bool -> ExpHF b -> ExpHF b -> ExpHF b
__caseTrue = caseHF0 "Prelude.True"

__caseFalse :: ExpHF Bool -> ExpHF b -> ExpHF b -> ExpHF b
__caseFalse = caseHF0 "Prelude.False"

__mkTrue :: ExpHF Bool
__mkTrue = conHF0 "Prelude.True"

__mkFalse :: ExpHF Bool
__mkFalse = conHF0 "Prelude.False"

__prim_sub_Integer :: ExpHF (Integer ->  Integer -> Integer)
__prim_sub_Integer = ExpHF $ primEH sub_IntegerP (arrowsT [integerT, integerT, integerT])

__prim_eq_Integer :: ExpHF (Integer -> Integer -> Bool)
__prim_eq_Integer = ExpHF $ primEH eq_IntegerP (arrowsT [integerT, integerT, boolT])

integerHF :: Integer -> ExpHF Integer
integerHF = ExpHF . integerEH

----------------------------

foldr :: ExpHF ((b -> a -> a) -> a -> [b] -> a)
foldr = lamHF "f" $ \f -> lamHF "z" $ \z -> lamHF "l" $ \l ->
    __caseCons__ l
       (lamHF "x" (\x -> lamHF "xs" (\xs ->
          applyHF (applyHF f x) (applyHF (applyHF (applyHF foldr f) z) xs))))
       z

replicate :: ExpHF (Integer -> a -> [a])
replicate = lamHF "n" (\n -> lamHF "x" (\x ->
  let _s2 = applyHF (applyHF __mkCons__ x) (applyHF (applyHF replicate (applyHF (applyHF __prim_sub_Integer n) (integerHF 1))) x)
  in __caseTrue (applyHF (applyHF __prim_eq_Integer (integerHF 0)) n) __mkNil__ _s2))

and :: ExpHF ([Bool] -> Bool)
and = applyHF (applyHF foldr (&&)) __mkTrue

(&&) :: ExpHF (Bool -> Bool -> Bool)
(&&) = lamHF "a" (\a -> lamHF "b" (\b -> __caseTrue a b __mkFalse))

result :: ExpHF Bool
result = applyHF and elems

elems = applyHF (applyHF replicate (integerHF 400000)) __mkTrue

main :: Prelude.IO ()
main = putStrLn $ show (unbox result)

