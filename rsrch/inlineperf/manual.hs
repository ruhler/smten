
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

import Debug.Trace

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.ExpH

lamHF :: (ExpH -> ExpH) -> ExpH
lamHF f =
  let s = error "lam sig"
      bt = error "lam bt"
  in lamEH s bt f

conHF :: String -> [ExpH] -> ExpH
conHF n xs = aconEH (name n) undefined xs

caseHF :: String -> ExpH -> ExpH -> ExpH -> ExpH
caseHF nm x y n =
 let t = error "caseHF t"
     tcs = error "caseHF tcs"
     s = Sig (name nm) t
 in caseEH tcs x s y n

eq :: ExpH
eq = lamHF $ \a -> lamHF $ \b ->
        case (de_integerEH a, de_integerEH b) of
            (Just av, Just bv) -> boolEH (av == bv)

sub :: ExpH
sub = lamHF $ \a -> lamHF $ \b ->
        case (de_integerEH a, de_integerEH b) of
            (Just av, Just bv) -> integerEH (av - bv)

mklist :: ExpH
mklist = lamHF $ \i ->
            lamHF $ \v ->
               ifEH undefined (appEH (appEH eq (integerEH 0)) i)
                          (conHF "Prelude.[]" [])
                          (conHF "Prelude.:" [v, appEH (appEH mklist (appEH (appEH sub i) (integerEH 1))) v]) 
                      
myand :: ExpH
myand = lamHF $ \l ->
            caseHF "Prelude.:" l (lamHF $ \x ->
                lamHF $ \xs ->
                   ifEH undefined x (appEH myand xs) falseEH)
                trueEH
                                         
elems :: ExpH
elems = appEH (appEH mklist (integerEH 1000000)) trueEH

result :: ExpH
result = appEH myand elems

main :: IO ()
main = do
    putStrLn $ show result

