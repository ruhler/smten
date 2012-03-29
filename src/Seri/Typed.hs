
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seri.Typed 
    (
        TypedExp(..),
        integerE, ifE, varE, varE_typed, lamE, appE,
        infixE,
        addP, subP, mulP, ltP, trueP, falseP, fixP,
    )
    where

import qualified Language.Haskell.TH as TH

import Seri.IR

data TypedExp a = TypedExp {
    typed :: Exp
}

class SeriType a where
    seritype :: a -> Type

instance SeriType Bool where
    seritype _ = BoolT

instance SeriType Integer where
    seritype _ = IntegerT

instance (SeriType a, SeriType b) => SeriType (a -> b) where
    seritype _ = 
        let xa = undefined :: a
            xb = undefined :: b
        in ArrowT (seritype xa) (seritype xb)

-- withtype f 
--  Calls the function f with the Type corresponding to the type of the
--  returned expression.
withtype :: (SeriType a) => (Type -> TypedExp a) -> TypedExp a
withtype f =
    let tt :: TypedExp a -> a
        tt _ = undefined
        r = f (seritype (tt r))
    in r

primitive :: (SeriType a) => Primitive -> TypedExp a
primitive p = withtype $ \t -> TypedExp $ PrimE t p

addP :: TypedExp (Integer -> Integer -> Integer)
addP = primitive AddP

subP :: TypedExp (Integer -> Integer -> Integer)
subP = primitive SubP

mulP :: TypedExp (Integer -> Integer -> Integer)
mulP = primitive MulP

ltP :: TypedExp (Integer -> Integer -> Bool)
ltP = primitive LtP

trueP :: TypedExp Bool
trueP = primitive TrueP

falseP :: TypedExp Bool
falseP = primitive FalseP

fixP :: (SeriType a) => TypedExp ((a -> a) -> a)
fixP = primitive FixP

integerE :: Integer -> TypedExp Integer
integerE x = TypedExp $ IntegerE x

ifE :: (SeriType a) => TypedExp Bool -> TypedExp a -> TypedExp a -> TypedExp a
ifE (TypedExp p) (TypedExp a) (TypedExp b)
    = withtype $ \t -> TypedExp $ IfE t p a b

appE :: (SeriType b) => TypedExp (a -> b) -> TypedExp a -> TypedExp b
appE (TypedExp f) (TypedExp x)
    = withtype $ \t -> TypedExp $ AppE t f x

lamE :: (SeriType a, SeriType (a -> b)) => Name -> (TypedExp a -> TypedExp b) -> TypedExp (a -> b)
lamE n f = withtype $ \t -> TypedExp $ LamE t n (typed $ f (varE n))

varE :: (SeriType a) => Name -> TypedExp a
varE nm = withtype $ \t -> TypedExp $ VarE t nm

-- varE_typed ref nm
-- Construct a variable whose type is the same as the type of 'ref'. 'ref' is
-- otherwise unused.
varE_typed :: (SeriType a) => TypedExp a -> Name -> TypedExp a
varE_typed _ = varE


infixE :: (SeriType b, SeriType c) => TypedExp (a -> b -> c) -> TypedExp a -> TypedExp b -> TypedExp c
infixE p a b = appE (appE p a) b

