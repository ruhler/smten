{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

import Prelude hiding (error, foldr, replicate, (&&), and, Num(..), Eq(..), not, Bool(..), Integer)
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude
import Smten.HaskellF.Lib.Prelude as Smten.Lib.Prelude

class HaskellF a => Num a
    where (+) :: Function a (Function a a)
          (-) :: Function a (Function a a)
          fromInteger :: Function Integer a

instance Num Integer
    where (+) = __prim_add_Integer
          (-) = __prim_sub_Integer
          fromInteger = lamHF "x" (\x -> x :: Integer)

class HaskellF a => Eq a
    where (==) :: Function a (Function a Bool)
          (==) = lamHF "a" (\a -> lamHF "b" (\b -> applyHF not (applyHF (applyHF (/=) a) b)))
          (/=) :: Function a (Function a Bool)
          (/=) = lamHF "a" (\a -> lamHF "b" (\b -> applyHF not (applyHF (applyHF (==) a) b)))

not :: Function Bool Bool
not = lamHF "_p" (\_p -> __caseTrue (_p :: Bool) __mkFalse __mkTrue)

instance Eq Integer
    where (==) = Smten.Lib.Prelude.__prim_eq_Integer :: Function Integer (Function Integer Bool)

foldr :: forall a b . (HaskellF a, HaskellF b) => Function (Function a (Function b b)) (Function b (Function (List__ a) b))
foldr = lamHF "_p" (\_p -> lamHF "_p1" (\_p1 -> lamHF "_p2" (\_p2 -> let _s2 :: b = applyHF (lamHF "f" (\f -> applyHF (lamHF "z" (\z -> z :: b)) (_p1 :: b))) (_p :: Function a (Function b b))
                                                                     in applyHF (lamHF "f" (\f -> applyHF (lamHF "z" (\z -> __caseCons__ (_p2 :: List__ a) (lamHF "x" (\x -> lamHF "xs" (\xs -> applyHF (applyHF (f :: Function a (Function b b)) (x :: a)) (applyHF (applyHF (applyHF (foldr :: Function (Function a (Function b b)) (Function b (Function (List__ a) b))) (f :: Function a (Function b b))) (z :: b)) (xs :: List__ a))))) (_s2 :: b))) (_p1 :: b))) (_p :: Function a (Function b b)))))
replicate :: forall a . HaskellF a => Function Integer (Function a (List__ a))
replicate = lamHF "_p" (\_p -> lamHF "_p1" (\_p1 -> let _s2 :: List__ a = applyHF (lamHF "n" (\n -> applyHF (lamHF "x" (\x -> applyHF (applyHF __mkCons__ (x :: a)) (applyHF (applyHF (replicate :: Function Integer (Function a (List__ a))) (applyHF (applyHF (((-)) :: Function Integer (Function Integer Integer)) (n :: Integer)) (applyHF (fromInteger :: Function Integer Integer) 1))) (x :: a)))) (_p1 :: a))) (_p :: Integer)
                                                    in __caseTrue (applyHF (applyHF (((==)) :: Function Integer (Function Integer Bool)) (applyHF (fromInteger :: Function Integer Integer) 0)) (_p :: Integer)) __mkNil__ (_s2 :: List__ a)))

and :: Function (List__ Bool) Bool
and = applyHF (applyHF foldr (&&)) __mkTrue

(&&) :: Function Bool (Function Bool Bool)
(&&) = lamHF "_p" (\_p -> lamHF "_p1" (\_p1 -> __caseTrue (_p :: Bool) (applyHF (lamHF "x" (\x -> x :: Bool)) (_p1 :: Bool)) __mkFalse))

result :: Bool
result = applyHF and elems

elems = applyHF (applyHF replicate 400000) __mkTrue

main :: Prelude.IO ()
main = do
   putStrLn $ show (unbox result) 

