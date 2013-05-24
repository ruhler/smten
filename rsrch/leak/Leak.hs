
{-# LANGUAGE FlexibleInstances #-}

import Smten.Name
import Smten.Sig
import Smten.Type
import Smten.ExpH
import Smten.Prim

import Prelude hiding (foldr, replicate, (&&), and, Bool(..), Integer) 

class (SmtenT a) => HaskellF a where
    box :: ExpH -> a
    unbox :: a -> ExpH

class (SmtenT1 m) => HaskellF1 m where
    box1 :: (HaskellF a) => ExpH -> m a
    unbox1 :: (HaskellF a) => m a -> ExpH 

class (SmtenT2 m) => HaskellF2 m where
    box2 :: (HaskellF a, HaskellF b) => ExpH -> m a b
    unbox2 :: (HaskellF a, HaskellF b) => m a b -> ExpH

instance (HaskellF1 m, HaskellF a) => HaskellF (m a) where
    box = box1
    unbox = unbox1

instance (HaskellF2 m, HaskellF a) => HaskellF1 (m a) where
    box1 = box2
    unbox1 = unbox2



newtype Function a b = Function {
    function_unbox :: ExpH
}

instance SmtenT2 Function where
    smtenT2 x = conT arrowN

instance HaskellF2 Function where
    box2 = Function
    unbox2 = function_unbox

applyHF :: (HaskellF a, HaskellF b) => Function a b -> a -> b
applyHF f x = box ({-# SCC "appHF0" #-} appEH ({-# SCC "appHF_fun" #-} unbox f) ({-# SCC "appHF_arg" #-}unbox x))

lamHF :: (HaskellF a, HaskellF b) => String -> (a -> b) -> Function a b
lamHF n f =
  let g :: ExpH -> ExpH
      g x = {-# SCC "lamHF_G" #-} unbox ({-# SCC "lamHF_G2" #-} ({-# SCC "lamHF_GF" #-} f) ({-# SCC "lamHF_G3" #-} box ({-# SCC "lamHF_G4" #-} x)))

      r = box $ lamEH ({-# SCC "lamHF_Sig" #-} Sig (name n) ta) ({-# SCC "lamHF_tb" #-} tb) g
      Just (ta, tb) = de_arrowT (smtenT r)
  in r

smtenHF :: (SmtenEH c, HaskellF f) => c -> f
smtenHF = box . smtenEH


primHF :: (HaskellF a) => Prim -> a
primHF p = 
 let z = box $ primEH p (smtenT z)
 in z

conHF' :: (HaskellF a) => String -> [ExpH] -> a
conHF' nm args =
  let r = box $ aconEH (name nm) (smtenT r) args
  in r

caseHF :: (HaskellF x, HaskellF y, HaskellF n) => String -> x -> y -> n -> n
caseHF k x y n =
  let tys = de_arrowsT $ smtenT y
      tcs = smtenT n
      tns = de_arrowsT tcs
      r = box $ caseEH tcs (unbox x) (Sig (name k) t) (unbox y) (unbox n)
      tx = smtenT x
      t = arrowsT (take (length tys - length tns) tys ++ [tx])
  in r


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

