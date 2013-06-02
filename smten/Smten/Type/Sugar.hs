
{-# LANGUAGE PatternGuards #-}

-- Abstract constructors and deconstructors for manipulating smten types.
module Smten.Type.Sugar (
    Type(),
    conT, de_conT, varT, 
    appT, de_appT, appsT, de_appsT,
    arrowT, de_arrowT, arrowsT, de_arrowsT,

    unitT, boolT, charT, integerT, listT, de_listT, stringT,
    bitT, de_bitT,
    tupleT, de_tupleT,

    addNT, subNT, mulNT,
  ) where

import Control.Monad(guard)
import Data.List(genericLength)

import Smten.Name
import Smten.Type.Type

-- | Form the type constructor with given name.
conT :: Name -> Type
conT n = ConT n UnknownK

varT :: Name -> Type
varT n = VarT n UnknownK

de_conT :: Type -> Maybe Name
de_conT (ConT n _) = Just n
de_conT _ = Nothing

-- | Type application.
-- Given a, b, returns the type (a b)
--
-- Special case: if the result would be an application of
--  ConT "+", ConT "-", or ConT "*" and two numeric types, converts it to the
--  numeric type operation.
--
-- This is to get the haskellf translation work with smtenT and numeric types.
appT :: Type -> Type -> Type
appT (AppT (ConT n _) a) b
 | n `elem` map name ntops = OpT (unname n) a b
appT a b = AppT a b

de_appT :: Type -> Maybe (Type, Type)
de_appT (AppT a b) = Just (a, b)
de_appT _ = Nothing

-- | Multi-arg type application
-- Given f, [a, b, ..., ], returns the type (f a b ...)
appsT :: Type -> [Type] -> Type
appsT t = foldl appT t

-- | Deconstruction of type application.
-- Given the type (f a b ...), returns (f, [a, b, ...])
de_appsT :: Type -> (Type, [Type])
de_appsT (AppT a b) =
  let (f, as) = de_appsT a
  in (f, as ++ [b])
de_appsT t = (t, [])

-- | Given types a, b, return type (a -> b)
arrowT :: Type -> Type -> Type
arrowT a b = appsT (conT arrowN) [a, b]

-- | Given a type of the form (a -> b), return (a, b).
de_arrowT :: Type -> Maybe (Type, Type)
de_arrowT (AppT (AppT (ConT ar _) a) b) | ar == arrowN = Just (a, b)
de_arrowT _ = Nothing

-- | Given types [a, b, ..., c], return type (a -> b -> ... -> c)
-- The given list must not be empty.
arrowsT :: [Type] -> Type
arrowsT [] = error $ "arrowsT applied to empty list"
arrowsT xs = foldr1 arrowT xs

-- | Given type (a -> b -> ... -> c), return [a, b, ..., c]
de_arrowsT :: Type -> [Type]
de_arrowsT t | Just (a, b) <- de_arrowT t = a : (de_arrowsT b)
de_arrowsT t = [t]

unitT :: Type
unitT = conT unitN

charT :: Type
charT = conT charN

integerT :: Type
integerT = conT integerN

boolT :: Type
boolT = conT boolN

-- | Given a type a, returns the type [a].
listT :: Type -> Type
listT t = appT (conT listN) t

de_listT :: Type -> Maybe Type
de_listT t = do
    (l, v) <- de_appT t
    n <- de_conT l
    guard $ n == listN
    return v

stringT :: Type
stringT = listT charT

bitT :: Integer -> Type
bitT w = appT (conT bitN) (NumT w)

de_bitT :: Type -> Maybe Integer
de_bitT t = do
  (k, w) <- de_appT t
  n <- de_conT k
  guard $ n == bitN
  return (nteval w)

-- | (a, b, ...)
-- There must be at least one type given.
--
-- If exactly one type is given, that type is returned without tupling.
tupleT :: [Type] -> Type
tupleT [] = error $ "tupT on empty list"
tupleT [x] = x
tupleT es = appsT (conT $ tupleN (length es)) es

de_tupleT :: Type -> Maybe [Type]
de_tupleT t =
 case de_appsT t of
    (ConT tn _, ts) -> do
        len <- de_tupleN tn
        guard $ len == genericLength ts
        return ts
    _ -> Nothing

addNT :: Type -> Type -> Type
addNT = OpT "+" 

subNT :: Type -> Type -> Type
subNT = OpT "-" 

mulNT :: Type -> Type -> Type
mulNT = OpT "*" 

