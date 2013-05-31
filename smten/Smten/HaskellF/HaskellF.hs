
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.HaskellF.HaskellF (
    ExpHF, box, unbox, T__Function,
    lamHF, applyHF, conHF, caseHF, primHF, mainHF, integerHF,
    smtenHF, de_smtenHF,
    ) where

import Smten.Name
import Smten.Type
import Smten.ExpH
import Smten.Prim

newtype ExpHF a = ExpHF {
    unbox :: ExpH
}

box :: ExpH -> ExpHF a
box = ExpHF

data T__Function a b

instance SmtenT2 T__Function where
    smtenT2 x = conT arrowN

smtenTHF :: (SmtenT a) => ExpHF a -> Type
smtenTHF x = {-# SCC "SmtenTHF" #-}
  let f :: ExpHF a -> a
      f _ = undefined
  in smtenT (f x)

applyHF :: (SmtenT a, SmtenT b) => ExpHF (T__Function a b) -> ExpHF a -> ExpHF b
applyHF f x =
  let Just (_, t) = de_arrowT (smtenTHF f)
  in box (appEH t (unbox f) (unbox x))

lamHF :: (SmtenT a, SmtenT b) => (ExpHF a -> ExpHF b) -> ExpHF (T__Function a b)
lamHF f =
  let g = \x -> unbox (f (box x))
      r = box $ lamEH t g
      t = smtenTHF r
  in r

smtenHF :: (SmtenEH c, SmtenT f) => c -> ExpHF f
smtenHF = box . smtenEH

de_smtenHF :: (SmtenEH c, SmtenT f) => ExpHF f -> Maybe c
de_smtenHF = de_smtenEH . unbox

conHF :: (SmtenT a) => Name -> [ExpH] -> ExpHF a
conHF k args =
  let r = box $ conEH (smtenTHF r) k args
  in r

caseHF :: (SmtenT x, SmtenT y, SmtenT n)
         => Name -> ExpHF x -> ExpHF y -> ExpHF n -> ExpHF n
caseHF k x y n = box $ caseEH (smtenTHF n) (unbox x) k (unbox y) (unbox n)

primHF :: (SmtenT a) => Prim -> ExpHF a
primHF f =
 let z = box $ f (smtenTHF z)
 in z

mainHF :: (SmtenT a) => ExpHF a -> IO ()
mainHF x
  | Just v <- de_ioEH (unbox x) = v >> return ()
  | otherwise = error "mainHF: main failed to compute"

integerHF :: (SmtenT a) => Integer -> ExpHF a
integerHF = smtenHF

