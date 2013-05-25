
{-# LANGUAGE FlexibleInstances #-}

import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH
import Smten.Prim

import Prelude hiding (foldr, replicate, (&&), and, Bool(..), Integer) 

applyHF :: ExpH -> ExpH -> ExpH
applyHF = appEH

lamHF :: String -> (ExpH -> ExpH) -> ExpH
lamHF _ f =
  let s = error "lamHF sig"
      tb = error "lamHF tb"
  in lamEH s tb f

conHF' :: String -> [ExpH] -> ExpH
conHF' nm args = aconEH (name nm) (error "conHF' t") args

caseHF :: String -> ExpH -> ExpH -> ExpH -> ExpH
caseHF k x y n = caseEH (error "tcs") x (Sig (name k) (error "caseHF t")) y n

__caseNil__ :: ExpH -> ExpH -> ExpH -> ExpH
__caseNil__ = caseHF "Prelude.[]"

__caseCons__ :: ExpH -> ExpH -> ExpH -> ExpH
__caseCons__ = caseHF "Prelude.:"

__mkNil__ :: ExpH
__mkNil__ = conHF' "Prelude.[]" []

__mkCons__ :: ExpH
__mkCons__ = lamHF "x1" $ \x1 ->
                lamHF "x2" $ \x2 ->
                  conHF' "Prelude.:" [x1, x2]

__caseTrue :: ExpH -> ExpH -> ExpH -> ExpH
__caseTrue = caseHF "Prelude.True"

__caseFalse :: ExpH -> ExpH -> ExpH -> ExpH
__caseFalse = caseHF "Prelude.False"

__mkTrue :: ExpH
__mkTrue = conHF' "Prelude.True" []

__mkFalse :: ExpH
__mkFalse = conHF' "Prelude.False" []

__prim_sub_Integer :: ExpH
__prim_sub_Integer = primEH sub_IntegerP (arrowsT [integerT, integerT, integerT])

__prim_eq_Integer :: ExpH
__prim_eq_Integer = primEH eq_IntegerP (arrowsT [integerT, integerT, boolT])


foldr :: ExpH
foldr = lamHF "f" $ \f -> lamHF "z" $ \z -> lamHF "l" $ \l ->
    __caseCons__ l
       (lamHF "x" (\x -> lamHF "xs" (\xs ->
          applyHF (applyHF f x) (applyHF (applyHF (applyHF foldr f) z) xs))))
       z

replicate :: ExpH
replicate = lamHF "n" (\n -> lamHF "x" (\x ->
  let _s2 = applyHF (applyHF __mkCons__ x) (applyHF (applyHF replicate (applyHF (applyHF __prim_sub_Integer n) (integerEH 1))) x)
  in __caseTrue (applyHF (applyHF __prim_eq_Integer (integerEH 0)) n) __mkNil__ _s2))

and :: ExpH
and = applyHF (applyHF foldr (&&)) __mkTrue

(&&) :: ExpH
(&&) = lamHF "a" (\a -> lamHF "b" (\b -> __caseTrue a b __mkFalse))

result :: ExpH
result = applyHF and elems

elems = applyHF (applyHF replicate (integerEH 400000)) __mkTrue

main :: Prelude.IO ()
main = putStrLn $ show result

