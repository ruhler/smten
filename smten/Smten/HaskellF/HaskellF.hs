
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.HaskellF.HaskellF (
    ExpHF, box, unbox, T__Function,
    lamHF, applyHF, conHF', de_conHF, caseHF, primHF, mainHF, integerHF,
    smtenHF, de_smtenHF,
    ) where

import Smten.Name
import Smten.Type
import Smten.Sig
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
smtenTHF x =
  let f :: ExpHF a -> a
      f _ = undefined
  in smtenT (f x)

applyHF :: (SmtenT a, SmtenT b) => ExpHF (T__Function a b) -> ExpHF a -> ExpHF b
applyHF f x =
  let Just (_, t) = de_arrowT (smtenTHF f)
  in box (appEH t (unbox f) (unbox x))

lamHF :: (SmtenT a, SmtenT b) => String -> (ExpHF a -> ExpHF b) -> ExpHF (T__Function a b)
lamHF n f =
  let g = {-# SCC "lamHF.g" #-} \x -> unbox (f (box x))
      r = {-# SCC "lamHF.r" #-} box $ lamEH t (name n) g
      t = {-# SCC "lamHF.t" #-} smtenTHF r
  in r

smtenHF :: (SmtenEH c, SmtenT f) => c -> ExpHF f
smtenHF = box . smtenEH

de_smtenHF :: (SmtenEH c, SmtenT f) => ExpHF f -> Maybe c
de_smtenHF = de_smtenEH . unbox

conHF' :: (SmtenT a) => String -> [ExpH] -> ExpHF a
conHF' nm args =
  let r = box $ aconEH (smtenTHF r) (name nm) args
  in r

de_conHF :: String -> ExpH -> Maybe [ExpH]
de_conHF nm = de_kconEH (name nm)

caseHF :: (SmtenT x, SmtenT y, SmtenT n)
         => String -> ExpHF x -> ExpHF y -> ExpHF n -> ExpHF n
caseHF k x y n = box $ caseEH (smtenTHF n) (unbox x) (name k) (unbox y) (unbox n)

primHF :: (SmtenT a) => Prim -> ExpHF a
primHF p = 
 let z = box $ primEH p (smtenTHF z)
 in z

mainHF :: (SmtenT a) => ExpHF a -> IO ()
mainHF x
  | Just v <- de_ioEH (unbox x) = v >> return ()
  | otherwise = error "mainHF: main failed to compute"


integerHF :: (SmtenT a) => Integer -> ExpHF a
integerHF = smtenHF

