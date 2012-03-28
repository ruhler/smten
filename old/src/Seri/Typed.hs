
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seri.Typed 
    (
        TypedExp(..),
        boolE, integerE, addE, subE, mulE, ltE, ifE,
        varE, lamE, appE, fixE
    )
    where

import Seri.IR

data TypedExp a = TypedExp {
    typed :: Exp
}

class SeriType a where
    seritype :: TypedExp a -> Type

instance SeriType Bool where
    seritype _ = BoolT

instance SeriType Integer where
    seritype _ = IntegerT

instance (SeriType a, SeriType b) => SeriType (a -> b) where
    seritype f = 
        let ta = varE "a" :: TypedExp a
            tb = varE "b" :: TypedExp b
        in ArrowT (seritype ta) (seritype tb)

boolE :: Bool -> TypedExp Bool
boolE x = TypedExp $ BoolE x

integerE :: Integer -> TypedExp Integer
integerE x = TypedExp $ IntegerE x

addE :: TypedExp Integer -> TypedExp Integer -> TypedExp Integer
addE (TypedExp a) (TypedExp b) = TypedExp (AddE a b)

subE :: TypedExp Integer -> TypedExp Integer -> TypedExp Integer
subE (TypedExp a) (TypedExp b) = TypedExp (SubE a b)

mulE :: TypedExp Integer -> TypedExp Integer -> TypedExp Integer
mulE (TypedExp a) (TypedExp b) = TypedExp (MulE a b)

ltE :: TypedExp Integer -> TypedExp Integer -> TypedExp Bool
ltE (TypedExp a) (TypedExp b) = TypedExp (LtE a b)

ifE :: (SeriType a) => TypedExp Bool -> TypedExp a -> TypedExp a -> TypedExp a
ifE (TypedExp p) ta@(TypedExp a) (TypedExp b)
    = TypedExp $ IfE (seritype ta) p a b

appE :: (SeriType b) => TypedExp (a -> b) -> TypedExp a -> TypedExp b
appE (TypedExp f) (TypedExp x) =
    let r = TypedExp (AppE (seritype r) f x)
    in r

lamE :: (SeriType a, SeriType (a -> b)) => Name -> (TypedExp a -> TypedExp b) -> TypedExp (a -> b)
lamE n f =
    let r = TypedExp (LamE (seritype r) n (typed $ f (varE n)))
    in r

fixE :: (SeriType a) => Name -> (TypedExp a -> TypedExp a) -> TypedExp a
fixE n f =
    let r = TypedExp $ FixE (seritype r) n (typed $ f (varE n))
    in r

varE :: (SeriType a) => Name -> TypedExp a
varE nm = 
    let r = TypedExp (VarE (seritype r) nm)
    in r


