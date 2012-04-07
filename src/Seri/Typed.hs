
{-# LANGUAGE FlexibleContexts #-}


module Seri.Typed 
    (
        Typed(..), SeriType(..),
        VarT_a(..), VarT_b(..), VarT_c(..), VarT_d(..),
    
        integerE, ifE, caseE, conE, conE_typed, varE, varE_typed, lamE, appE,
        infixE, primitive, match, lamM,
        conP, appP,
        addP, subP, mulP, ltP,
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

instance SeriType () where
    seritype _ = UnitT

instance SeriType Bool where
    seritype _ = ConT "Bool"

instance SeriType Integer where
    seritype _ = ConT "Integer"

instance (SeriType a, SeriType b) => SeriType (a -> b) where
    seritype f = 
        let ta :: (a -> b) -> a
            ta _ = undefined

            tb :: (a -> b) -> b
            tb _ = undefined
        in AppT (AppT ArrowT (seritype (ta f))) (seritype (tb f))

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
data VarT_a = VarT_a
data VarT_b = VarT_b
data VarT_c = VarT_c
data VarT_d = VarT_d

instance SeriType VarT_a where
    seritype _ = VarT "a"

instance SeriType VarT_b where
    seritype _ = VarT "b"

instance SeriType VarT_c where
    seritype _ = VarT "c"

instance SeriType VarT_d where
    seritype _ = VarT "d"

usetype :: (SeriType a) => Typed Exp a -> (Type -> b) -> b
usetype e f = f (seritype (gettype e))
    where gettype :: Typed Exp a -> a
          gettype _ = undefined

-- withtype f 
--  Calls the function f with the Type corresponding to the type of the
--  returned expression.
withtype :: (SeriType a) => (Type -> Typed Exp a) -> Typed Exp a
withtype f = r where r = usetype r f

primitive :: (SeriType a) => Primitive -> Typed Exp a
primitive p = withtype $ \t -> Typed $ PrimE t p

addP :: Typed Exp (Integer -> Integer -> Integer)
addP = primitive AddP

subP :: Typed Exp (Integer -> Integer -> Integer)
subP = primitive SubP

mulP :: Typed Exp (Integer -> Integer -> Integer)
mulP = primitive MulP

ltP :: Typed Exp (Integer -> Integer -> Bool)
ltP = primitive LtP

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

