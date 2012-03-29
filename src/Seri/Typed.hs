
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seri.Typed 
    (
        TypedExp(..),
        integerE, ifE, varE, varE_typed, lamE, appE,
        infixE,
        addP, subP, mulP, ltP,
        valD,
        _seri__True, _serictx_True,
        _seri__False, _serictx_False,
        _seri__fix, _serictx_fix,
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

usetype :: (SeriType a) => TypedExp a -> (Type -> b) -> b
usetype e f = f (seritype (gettype e))
    where gettype :: TypedExp a -> a
          gettype _ = undefined

-- withtype f 
--  Calls the function f with the Type corresponding to the type of the
--  returned expression.
withtype :: (SeriType a) => (Type -> TypedExp a) -> TypedExp a
withtype f = r where r = usetype r f

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

_seri__True :: TypedExp Bool
_seri__True = primitive TrueP

_serictx_True :: [Dec]
_serictx_True = [valD "True" _seri__True]

_seri__False :: TypedExp Bool
_seri__False = primitive FalseP

_serictx_False :: [Dec]
_serictx_False = [valD "False" _seri__False]

_seri__fix :: (SeriType a) => TypedExp ((a -> a) -> a)
_seri__fix = primitive FixP

_serictx_fix :: [Dec]
_serictx_fix
  = let fixtype = (ArrowT (ArrowT (VarT "a") (VarT "a")) (VarT "a"))
        dummyfix :: TypedExp ((Bool -> Bool) -> Bool)
        dummyfix = _seri__fix
    in [ValD "fix" fixtype (typed dummyfix)]

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

valD :: (SeriType a) => Name -> TypedExp a -> Dec
valD nm e = usetype e (\t -> ValD nm t (typed e))

