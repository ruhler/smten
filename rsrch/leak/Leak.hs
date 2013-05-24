{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

import Prelude hiding (foldr, replicate, (&&), and, Num(..), Eq(..), not)
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Lib.Prelude
import Smten.HaskellF.Lib.Prelude as Smten.Lib.Prelude

class Smten.HaskellF.HaskellF.HaskellF a => Num a
    where (+) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a)
          (+) = Smten.HaskellF.HaskellF.applyHF (Smten.Lib.Prelude.error :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.Lib.Prelude.Char) (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a))) (Smten.HaskellF.HaskellF.smtenHF "/home/ruhler/sri/projects/smten/build/home/.cabal/share/smten-2.1.0.0/lib/Prelude.smtn:49:4: no default implementation for (+ :: a -> a -> a)")
          (*) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a)
          (*) = Smten.HaskellF.HaskellF.applyHF (Smten.Lib.Prelude.error :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.Lib.Prelude.Char) (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a))) (Smten.HaskellF.HaskellF.smtenHF "/home/ruhler/sri/projects/smten/build/home/.cabal/share/smten-2.1.0.0/lib/Prelude.smtn:50:4: no default implementation for (* :: a -> a -> a)")
          (-) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a)
          (-) = Smten.HaskellF.HaskellF.applyHF (Smten.Lib.Prelude.error :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.Lib.Prelude.Char) (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a a))) (Smten.HaskellF.HaskellF.smtenHF "/home/ruhler/sri/projects/smten/build/home/.cabal/share/smten-2.1.0.0/lib/Prelude.smtn:51:9: no default implementation for (- :: a -> a -> a)")
          negate :: Smten.HaskellF.HaskellF.Function a a
          negate = Smten.HaskellF.HaskellF.applyHF (Smten.Lib.Prelude.error :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.Lib.Prelude.Char) (Smten.HaskellF.HaskellF.Function a a)) (Smten.HaskellF.HaskellF.smtenHF "/home/ruhler/sri/projects/smten/build/home/.cabal/share/smten-2.1.0.0/lib/Prelude.smtn:52:6: no default implementation for (negate :: a -> a)")
          fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer a
          fromInteger = Smten.HaskellF.HaskellF.applyHF (Smten.Lib.Prelude.error :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.Lib.Prelude.Char) (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer a)) (Smten.HaskellF.HaskellF.smtenHF "/home/ruhler/sri/projects/smten/build/home/.cabal/share/smten-2.1.0.0/lib/Prelude.smtn:55:6: no default implementation for (fromInteger :: Integer -> a)")

instance Num Smten.Lib.Prelude.Integer
    where (+) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          (+) = Smten.Lib.Prelude.__prim_add_Integer :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          (-) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          (-) = Smten.Lib.Prelude.__prim_sub_Integer :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          (*) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          (*) = Smten.Lib.Prelude.__prim_mul_Integer :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)
          negate :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer
          negate = Smten.HaskellF.HaskellF.lamHF "x" (\x -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (((-)) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)) (Smten.HaskellF.HaskellF.applyHF (fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer) 0)) (x :: Smten.Lib.Prelude.Integer))
          fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer
          fromInteger = Smten.HaskellF.HaskellF.lamHF "x" (\x -> x :: Smten.Lib.Prelude.Integer)

class Smten.HaskellF.HaskellF.HaskellF a => Eq a
    where (==) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a Smten.HaskellF.Lib.Prelude.Bool)
          (==) = Smten.HaskellF.HaskellF.lamHF "a" (\a -> Smten.HaskellF.HaskellF.lamHF "b" (\b -> Smten.HaskellF.HaskellF.applyHF (not :: Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (((/=)) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a Smten.HaskellF.Lib.Prelude.Bool)) (a :: a)) (b :: a))))
          (/=) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a Smten.HaskellF.Lib.Prelude.Bool)
          (/=) = Smten.HaskellF.HaskellF.lamHF "a" (\a -> Smten.HaskellF.HaskellF.lamHF "b" (\b -> Smten.HaskellF.HaskellF.applyHF (not :: Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (((==)) :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function a Smten.HaskellF.Lib.Prelude.Bool)) (a :: a)) (b :: a))))

not :: Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool
not = Smten.HaskellF.HaskellF.lamHF "_p" (\_p -> Smten.Lib.Prelude.__caseTrue (_p :: Smten.HaskellF.Lib.Prelude.Bool) Smten.Lib.Prelude.__mkFalse Smten.Lib.Prelude.__mkTrue)

instance Eq Smten.Lib.Prelude.Integer
    where (==) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.HaskellF.Lib.Prelude.Bool)
          (==) = Smten.Lib.Prelude.__prim_eq_Integer :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.HaskellF.Lib.Prelude.Bool)

foldr :: forall a b . (Smten.HaskellF.HaskellF.HaskellF a, Smten.HaskellF.HaskellF.HaskellF b) => Smten.HaskellF.HaskellF.Function (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b)) (Smten.HaskellF.HaskellF.Function b (Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ a) b))
foldr = Smten.HaskellF.HaskellF.lamHF "_p" (\_p -> Smten.HaskellF.HaskellF.lamHF "_p1" (\_p1 -> Smten.HaskellF.HaskellF.lamHF "_p2" (\_p2 -> let _s2 :: b = Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "f" (\f -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "z" (\z -> z :: b)) (_p1 :: b))) (_p :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b))
                                                                                                                                              in Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "f" (\f -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "z" (\z -> Smten.HaskellF.Lib.Prelude.__caseCons__ (_p2 :: Smten.HaskellF.Lib.Prelude.List__ a) (Smten.HaskellF.HaskellF.lamHF "x" (\x -> Smten.HaskellF.HaskellF.lamHF "xs" (\xs -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (f :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b)) (x :: a)) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (foldr :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b)) (Smten.HaskellF.HaskellF.Function b (Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ a) b))) (f :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b))) (z :: b)) (xs :: Smten.HaskellF.Lib.Prelude.List__ a))))) (_s2 :: b))) (_p1 :: b))) (_p :: Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.HaskellF.Function b b)))))
replicate :: forall a . Smten.HaskellF.HaskellF.HaskellF a => Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.Lib.Prelude.List__ a))
replicate = Smten.HaskellF.HaskellF.lamHF "_p" (\_p -> Smten.HaskellF.HaskellF.lamHF "_p1" (\_p1 -> let _s2 :: Smten.HaskellF.Lib.Prelude.List__ a = Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "n" (\n -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "x" (\x -> Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF Smten.HaskellF.Lib.Prelude.__mkCons__ (x :: a)) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (replicate :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function a (Smten.HaskellF.Lib.Prelude.List__ a))) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (((-)) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer)) (n :: Smten.Lib.Prelude.Integer)) (Smten.HaskellF.HaskellF.applyHF (fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer) 1))) (x :: a)))) (_p1 :: a))) (_p :: Smten.Lib.Prelude.Integer)
                                                                                                     in Smten.Lib.Prelude.__caseTrue (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (((==)) :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.HaskellF.Lib.Prelude.Bool)) (Smten.HaskellF.HaskellF.applyHF (fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer) 0)) (_p :: Smten.Lib.Prelude.Integer)) Smten.HaskellF.Lib.Prelude.__mkNil__ (_s2 :: Smten.HaskellF.Lib.Prelude.List__ a)))

and :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.HaskellF.Lib.Prelude.Bool) Smten.HaskellF.Lib.Prelude.Bool
and = Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (foldr :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool)) (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool (Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.HaskellF.Lib.Prelude.Bool) Smten.HaskellF.Lib.Prelude.Bool))) (((&&)) :: Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool))) Smten.Lib.Prelude.__mkTrue

(&&) :: Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool Smten.HaskellF.Lib.Prelude.Bool)
(&&) = Smten.HaskellF.HaskellF.lamHF "_p" (\_p -> Smten.HaskellF.HaskellF.lamHF "_p1" (\_p1 -> Smten.Lib.Prelude.__caseTrue (_p :: Smten.HaskellF.Lib.Prelude.Bool) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.lamHF "x" (\x -> x :: Smten.HaskellF.Lib.Prelude.Bool)) (_p1 :: Smten.HaskellF.Lib.Prelude.Bool)) Smten.Lib.Prelude.__mkFalse))

result :: Smten.HaskellF.Lib.Prelude.Bool
result = (Smten.HaskellF.HaskellF.applyHF (and :: Smten.HaskellF.HaskellF.Function (Smten.HaskellF.Lib.Prelude.List__ Smten.HaskellF.Lib.Prelude.Bool) Smten.HaskellF.Lib.Prelude.Bool) (Smten.HaskellF.HaskellF.applyHF (Smten.HaskellF.HaskellF.applyHF (replicate :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer (Smten.HaskellF.HaskellF.Function Smten.HaskellF.Lib.Prelude.Bool (Smten.HaskellF.Lib.Prelude.List__ Smten.HaskellF.Lib.Prelude.Bool))) (Smten.HaskellF.HaskellF.applyHF (fromInteger :: Smten.HaskellF.HaskellF.Function Smten.Lib.Prelude.Integer Smten.Lib.Prelude.Integer) 400000)) Smten.Lib.Prelude.__mkTrue))

main :: Prelude.IO ()
main = do
   putStrLn $ show (unbox result) 

