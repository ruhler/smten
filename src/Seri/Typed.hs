
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seri.Typed 
    (
        TypedExp(..),
        integerE, ifE, varE, lamE, appE,
        infixE,
        addP, subP, mulP, ltP, trueP, falseP, fixP
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

seritypeE :: (SeriType a) => TypedExp a -> Type
seritypeE x =
    let f :: TypedExp a -> a
        f _ = undefined
    in seritype (f x)


addP :: TypedExp (Integer -> Integer -> Integer)
addP = TypedExp (PrimE (ArrowT IntegerT (ArrowT IntegerT IntegerT)) AddP)

subP :: TypedExp (Integer -> Integer -> Integer)
subP = TypedExp (PrimE (ArrowT IntegerT (ArrowT IntegerT IntegerT)) SubP)

mulP :: TypedExp (Integer -> Integer -> Integer)
mulP = TypedExp (PrimE (ArrowT IntegerT (ArrowT IntegerT IntegerT)) MulP)

ltP :: TypedExp (Integer -> Integer -> Bool)
ltP = TypedExp (PrimE (ArrowT IntegerT (ArrowT IntegerT BoolT)) LtP)

trueP :: TypedExp Bool
trueP = TypedExp $ PrimE BoolT TrueP

falseP :: TypedExp Bool
falseP = TypedExp $ PrimE BoolT FalseP

fixP :: (SeriType a) => TypedExp ((a -> a) -> a)
fixP =
  let r = TypedExp $ PrimE (seritypeE r) FixP
  in r

integerE :: Integer -> TypedExp Integer
integerE x = TypedExp $ IntegerE x

ifE :: (SeriType a) => TypedExp Bool -> TypedExp a -> TypedExp a -> TypedExp a
ifE (TypedExp p) ta@(TypedExp a) (TypedExp b)
    = TypedExp $ IfE (seritypeE ta) p a b

appE :: (SeriType b) => TypedExp (a -> b) -> TypedExp a -> TypedExp b
appE (TypedExp f) (TypedExp x) =
    let r = TypedExp (AppE (seritypeE r) f x)
    in r

lamE :: (SeriType a, SeriType (a -> b)) => Name -> (TypedExp a -> TypedExp b) -> TypedExp (a -> b)
lamE n f =
    let r = TypedExp (LamE (seritypeE r) n (typed $ f (varE n)))
    in r

varE :: (SeriType a) => Name -> TypedExp a
varE nm = 
    let r = TypedExp (VarE (seritypeE r) nm)
    in r


infixE :: (SeriType b, SeriType c) => TypedExp (a -> b -> c) -> TypedExp a -> TypedExp b -> TypedExp c
infixE p a b = appE (appE p a) b

