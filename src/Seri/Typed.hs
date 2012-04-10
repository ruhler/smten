
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Seri.Typed 
    (
        Typed(..), SeriType(..), SeriType1(..), SeriType2(..),
        VarT_a(..), VarT_b(..), VarT_c(..), VarT_d(..), VarT_m(..),
    
        integerE, ifE, caseE, conE, conE_typed, varE, varE_typed, lamE, appE,
        infixE, primitive, match, lamM,
        conP, appP, wildP, integerP,
        valD,
    )
    where

import qualified Language.Haskell.TH as TH

import Seri.IR

data Typed x t = Typed {
    typed :: x
}

class SeriType a where
    seritype :: a -> Type

instance SeriType Integer where
    seritype _ = ConT "Integer"

class SeriType1 m where
    seritype1 :: m a -> Type

instance (SeriType1 m, SeriType a) => SeriType (m a) where  
    seritype m =
        let ta :: m a -> a
            ta _ = undefined
        in AppT (seritype1 m) (seritype (ta m))

class SeriType2 m where
    seritype2 :: m a b -> Type

instance SeriType2 (->) where
    seritype2 _ = ArrowT

instance (SeriType2 m, SeriType a) => SeriType1 (m a) where
    seritype1 ma =
        let ta :: m a b -> a
            ta _ = undefined
        in AppT (seritype2 ma) (seritype (ta ma))

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
data VarT_a = VarT_a
data VarT_b = VarT_b
data VarT_c = VarT_c
data VarT_d = VarT_d

-- And some of kind * -> *.
data VarT_m a = VarT_m

instance SeriType VarT_a where
    seritype _ = VarT "a"

instance SeriType VarT_b where
    seritype _ = VarT "b"

instance SeriType VarT_c where
    seritype _ = VarT "c"

instance SeriType VarT_d where
    seritype _ = VarT "d"

instance SeriType1 VarT_m where
    seritype1 _ = VarT "m"

instance Monad VarT_m where
    return = error $ "return VarT_m"
    (>>=) = error $ ">>= VarT_m"

usetype :: (SeriType a) => Typed Exp a -> (Type -> b) -> b
usetype e f = f (seritype (gettype e))
    where gettype :: Typed Exp a -> a
          gettype _ = undefined

-- withtype f 
--  Calls the function f with the Type corresponding to the type of the
--  returned expression.
withtype :: (SeriType a) => (Type -> Typed Exp a) -> Typed Exp a
withtype f = r where r = usetype r f

primitive :: (SeriType a) => Name -> Typed Exp a
primitive p = withtype $ \t -> Typed $ PrimE t p

integerE :: Integer -> Typed Exp Integer
integerE x = Typed $ IntegerE x

ifE :: (SeriType a) => Typed Exp Bool -> Typed Exp a -> Typed Exp a -> Typed Exp a
ifE (Typed p) (Typed a) (Typed b)
    = withtype $ \t -> Typed $ IfE t p a b

caseE :: (SeriType b) => Typed Exp a -> [Typed Match (a -> b)] -> Typed Exp b
caseE (Typed e) matches = withtype $ \t -> Typed $ CaseE t e (map typed matches)

match :: Typed Pat a -> Typed Exp b -> Typed Match (a -> b)
match (Typed p) (Typed e) = Typed $ Match p e

conP :: Name -> Typed Pat a
conP n = Typed $ ConP n

appP :: Typed Pat (a -> b) -> Typed Pat a -> Typed Pat b
appP (Typed f) (Typed x) = Typed $ AppP f x

wildP :: Typed Pat a
wildP = Typed WildP

integerP :: Integer -> Typed Pat Integer
integerP i = Typed $ IntegerP i

appE :: (SeriType b) => Typed Exp (a -> b) -> Typed Exp a -> Typed Exp b
appE (Typed f) (Typed x)
    = withtype $ \t -> Typed $ AppE t f x

lamE :: (SeriType a, SeriType (a -> b)) => Name -> (Typed Exp a -> Typed Exp b) -> Typed Exp (a -> b)
lamE n f = withtype $ \t -> Typed $ LamE t n (typed $ f (varE n))

-- Construct a type safe match which uses a variable pattern.
lamM :: (SeriType a) => Name -> (Typed Pat a -> Typed Exp a -> Typed Match b) -> Typed Match b
lamM n f = f (Typed $ VarP n) (varE n)

varE :: (SeriType a) => Name -> Typed Exp a
varE nm = withtype $ \t -> Typed $ VarE t nm

-- varE_typed ref nm
-- Construct a variable whose type is the same as the type of 'ref'. 'ref' is
-- otherwise unused.
varE_typed :: (SeriType a) => Typed Exp a -> Name -> Typed Exp a
varE_typed _ = varE

conE :: (SeriType a) => Name -> Typed Exp a
conE nm = withtype $ \t -> Typed $ ConE t nm

conE_typed :: (SeriType a) => Typed Exp a -> Name -> Typed Exp a
conE_typed _ = conE

infixE :: (SeriType b, SeriType c) => Typed Exp (a -> b -> c) -> Typed Exp a -> Typed Exp b -> Typed Exp c
infixE p a b = appE (appE p a) b

valD :: (SeriType a) => Name -> Typed Exp a -> Dec
valD nm e = usetype e (\t -> ValD nm t (typed e))

