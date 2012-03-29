
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Seri.Typed 
    (
        TypedExp(..),
        integerE, ifE, varE, varE_typed, lamE, appE,
        infixE,
        addP, subP, mulP, ltP,
        valD,
        _seriP_True, _seriC_True, _seriD_True,
        _seriP_False, _seriC_False, _seriD_False,
        _seriP_fix, _seriC_fix, _seriD_fix,
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

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
data VarT_a = VarT_a

instance SeriType VarT_a where
    seritype _ = VarT "a"

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

_seriP_True :: TypedExp Bool
_seriP_True = primitive TrueP

_seriC_True :: TypedExp Bool
_seriC_True = _seriP_True

_seriD_True :: [Dec]
_seriD_True = [valD "True" _seriC_True]

_seriP_False :: TypedExp Bool
_seriP_False = primitive FalseP

_seriC_False :: TypedExp Bool
_seriC_False = _seriP_False

_seriD_False :: [Dec]
_seriD_False = [valD "False" _seriC_False]

_seriP_fix :: (SeriType a) => TypedExp ((a -> a) -> a)
_seriP_fix = primitive FixP

_seriC_fix :: TypedExp ((VarT_a -> VarT_a) -> VarT_a)
_seriC_fix = _seriP_fix

_seriD_fix :: [Dec]
_seriD_fix = [valD "fix" _seriC_fix]

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

