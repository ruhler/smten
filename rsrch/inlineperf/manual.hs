
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Prelude as P
import Debug.Trace

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.ExpH
import Smten.Prim
import Smten.Ppr

newtype ExpHF a = ExpHF { 
    unbox :: ExpH
}

class TypeHF a where
    typeHF :: ExpHF a -> Type

class TypeHF1 m where
    typeHF1 :: ExpHF (m a) -> Type

instance (TypeHF a, TypeHF1 m) => TypeHF (m a) where
    typeHF x =
      let fta :: ExpHF (m a) -> ExpHF a
          fta _ = P.undefined
      in appT (typeHF1 x) (typeHF (fta x))

class TypeHF2 m where
    typeHF2 :: ExpHF (m a b) -> Type

instance (TypeHF a, TypeHF2 m) => TypeHF1 (m a) where
    typeHF1 x =
      let fta :: ExpHF (m a b) -> ExpHF a
          fta _ = P.undefined
      in appT (typeHF2 x) (typeHF (fta x))


appHF :: ExpHF (FunT a b) -> ExpHF a -> ExpHF b
appHF f x = ExpHF (appEH (unbox f) (unbox x))

lamHF :: (TypeHF a, TypeHF b) => (ExpHF a -> ExpHF b) -> ExpHF (FunT a b)
lamHF f =
  let g :: ExpH -> ExpH
      g x = unbox (f (ExpHF x))

      s = P.error "lam sig"
      bt = P.error "lam bt"
  in ExpHF (lamEH s bt g)

conHF :: (TypeHF a) => P.String -> ExpHF a
conHF n = 
  let r = ExpHF (conEH (Sig (name n) t))
      t = typeHF r
  in r

caseHF :: (TypeHF a, TypeHF b, TypeHF c) => P.String -> ExpHF a -> ExpHF b -> ExpHF c -> ExpHF c
caseHF nm x y n =
 let t = P.error "caseHF t"
     tcs = P.error "caseHF tcs"
     arg = {-# SCC "caseHF_X" #-} unbox x
     yes = {-# SCC "caseHF_Y" #-} unbox y
     no = {-# SCC "caseHF_N" #-} unbox n
     s = {-# SCC "caseHF_SIG" #-} Sig (name nm) t
 in ExpHF ({-# SCC "caseHF_caseEH" #-} caseEH tcs arg s yes no)

mainHF :: ExpHF (IoT UnitT) -> P.IO ()
mainHF x = case de_ioEH (unbox x) of
              P.Just x -> x P.>> P.return ()
              _ -> P.error P.$ "mainHF not concrete: " P.++ pretty (unbox x)

charHF :: P.Char -> ExpHF CharT
charHF c = ExpHF (charEH c)

integerHF :: P.Integer -> ExpHF IntegerT
integerHF i = ExpHF (integerEH i)

stringHF :: P.String -> ExpHF (ListT CharT)
stringHF [] = __mkNil
stringHF (c:cs) = appHF (appHF __mkCons (charHF c)) (stringHF cs)

primHF :: (TypeHF a) => Prim -> ExpHF a
primHF p =
  let r = ExpHF (primEH p t)
      t = typeHF r
  in r


data FunT a b
instance TypeHF2 FunT where
    typeHF2 _ = conT arrowN

data BoolT
instance TypeHF BoolT where
    typeHF _ = boolT

__caseTrue :: (TypeHF a) => ExpHF BoolT -> ExpHF a -> ExpHF a -> ExpHF a
__caseTrue = caseHF "Prelude.True"

__caseFalse :: (TypeHF a) => ExpHF BoolT -> ExpHF a -> ExpHF a -> ExpHF a
__caseFalse = caseHF "Prelude.False"

__mkTrue :: ExpHF BoolT
__mkTrue = conHF "Prelude.True"

__mkFalse :: ExpHF BoolT
__mkFalse = conHF "Prelude.False"

data ListT a

instance TypeHF1 ListT where
    typeHF1 _ = conT listN

__caseNil :: (TypeHF a, TypeHF b) => ExpHF (ListT a) -> ExpHF b -> ExpHF b -> ExpHF b
__caseNil = caseHF "Prelude.[]"

__caseCons :: (TypeHF a, TypeHF b) => ExpHF (ListT a) -> ExpHF (FunT a (FunT (ListT a) b)) -> ExpHF b -> ExpHF b
__caseCons = caseHF "Prelude.:"

__mkNil :: (TypeHF a) => ExpHF (ListT a)
__mkNil = conHF "Prelude.[]"

__mkCons :: (TypeHF a) => ExpHF (FunT a (FunT (ListT a) (ListT a)))
__mkCons = conHF "Prelude.:"

data UnitT

instance TypeHF UnitT where
    typeHF _ = unitT

__mkUnit :: ExpHF UnitT
__mkUnit = conHF "Prelude.()"

data CharT
instance TypeHF CharT where
    typeHF _ = charT

data IntegerT
instance TypeHF IntegerT where
    typeHF _ = integerT

data IoT a

instance TypeHF1 IoT where
    typeHF1 _ = conT ioN
    

class (TypeHF a) => Eq a where
    (==) :: ExpHF (FunT a (FunT a BoolT))

class (TypeHF a) => Num a where
    (-) :: ExpHF (FunT a (FunT a a))

class (TypeHF a) => Show a where
    show :: ExpHF (FunT a (ListT CharT))

class (TypeHF1 m) => Monad m where
    return :: (TypeHF a) => ExpHF (FunT a (m a))
    (>>=) :: (TypeHF a, TypeHF b) => ExpHF (FunT (m a) (FunT (FunT a (m b)) (m b)))
    (>>) :: (TypeHF a, TypeHF b) => ExpHF (FunT (m a) (FunT (m b) (m b)))
    (>>) = lamHF (\x -> lamHF (\y ->
             appHF (appHF (>>=) x) (lamHF (\_ -> y))))

instance Monad IoT where
    return = primHF return_IOP
    (>>=) = primHF bind_IOP

instance Eq IntegerT where
    (==) = primHF eq_IntegerP

instance Num IntegerT where
    (-) = primHF sub_IntegerP

instance Show BoolT where
    show = lamHF (\x ->  
            __caseTrue x (stringHF "True") (stringHF "False"))

error :: (TypeHF a) => ExpHF (FunT (ListT CharT) a)
error = primHF errorP

putChar :: ExpHF (FunT CharT (IoT UnitT))
putChar = primHF putCharP

map :: (TypeHF a, TypeHF b) => ExpHF (FunT (FunT a b) (FunT (ListT a) (ListT b)))
map = lamHF (\f ->
      lamHF (\l ->
      __caseNil l __mkNil P.$
        __caseCons l (lamHF (\x -> lamHF (\xs ->
                appHF (appHF __mkCons (appHF f x)) (appHF (appHF map f) xs)
              )))
              (appHF error (stringHF "case no match"))))
           

sequence :: (TypeHF a, TypeHF1 m, Monad m) => ExpHF (FunT (ListT (m a)) (m (ListT a)))
sequence
  = lamHF P.$ \l ->
      __caseNil l (appHF return __mkNil) P.$
          __caseCons l (lamHF P.$ \x -> lamHF P.$ \xs ->
                appHF (appHF (>>=) x) (lamHF P.$ \v ->
                    appHF (appHF (>>=) (appHF sequence xs)) (lamHF P.$ \vs ->
                       appHF return (appHF (appHF __mkCons v) vs)))
              )
              (appHF error (stringHF "case no match"))
                    

mapM :: (TypeHF a, TypeHF b, TypeHF1 m, Monad m) => ExpHF (FunT (FunT a (m b)) (FunT (ListT a) (m (ListT b))))
mapM = lamHF P.$ \f ->
         lamHF P.$ \as ->
           appHF sequence (appHF (appHF map f) as)

mapM_ :: (Monad m, TypeHF a, TypeHF b, TypeHF1 m) => ExpHF (FunT (FunT a (m b)) (FunT (ListT a) (m UnitT)))
mapM_ = lamHF P.$ \f ->
          lamHF P.$ \as ->
            appHF (appHF (>>) (appHF (appHF mapM f) as)) (appHF return __mkUnit)

putStr :: ExpHF (FunT (ListT CharT) (IoT UnitT))
putStr = appHF mapM_ putChar

putStrLn :: ExpHF (FunT (ListT CharT) (IoT UnitT))
putStrLn = lamHF P.$ \str ->
              appHF (appHF (>>) (appHF putStr str))
                                (appHF putStr (stringHF "\n"))

mklist :: (TypeHF a) => ExpHF (FunT IntegerT (FunT a (ListT a)))
mklist = lamHF P.$ \i ->
            lamHF P.$ \v ->
               __caseTrue (appHF (appHF (==) (integerHF 0)) i)
                          (__mkNil)
                          (appHF (appHF __mkCons v) (appHF (appHF mklist (appHF (appHF (-) i) (integerHF 1))) v)) 
                      
myand :: ExpHF (FunT (ListT BoolT) BoolT)
myand = lamHF P.$ \l ->
            __caseCons l (lamHF P.$ \x ->
                            lamHF P.$ \xs ->
                                {-# SCC "LAMBDA_XS_BODY" #-}
                              __caseTrue x (appHF myand xs) __mkFalse)
                         __mkTrue
                                         
elems :: ExpHF (ListT BoolT)
elems = appHF (appHF mklist (integerHF 1000000)) __mkTrue

result :: ExpHF BoolT
result = appHF myand elems

main :: P.IO ()
main = mainHF P.$ appHF putStrLn (appHF show result)

