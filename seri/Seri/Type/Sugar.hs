
{-# LANGUAGE PatternGuards #-}

-- Abstract constructors and deconstructors for manipulating seri types.
module Seri.Type.Sugar (
    conT,
    appT, appsT, de_appsT,
    arrowN, arrowT, de_arrowT, arrowsT, de_arrowsT,

    unitT, boolT, charT, integerT, listT, stringT,
  ) where

import Seri.Name
import Seri.Type.Type

-- | Form the type constructor with given name.
conT :: Name -> Type
conT = ConT

-- | Type application.
-- Given a, b, returns the type (a b)
appT :: Type -> Type -> Type
appT = AppT

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

-- | The name of the (->) type constructor.
arrowN :: Name
arrowN = name "->"

-- | Given types a, b, return type (a -> b)
arrowT :: Type -> Type -> Type
arrowT a b = appsT (conT arrowN) [a, b]

-- | Given a type of the form (a -> b), return (a, b).
de_arrowT :: Type -> Maybe (Type, Type)
de_arrowT (AppT (AppT (ConT ar) a) b) | ar == arrowN = Just (a, b)
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
unitT = conT (name "()")

charT :: Type
charT = conT (name "Char")

integerT :: Type
integerT = conT (name "Integer")

boolT :: Type
boolT = conT (name "Bool")

-- | Given a type a, returns the type [a].
listT :: Type -> Type
listT t = appT (conT (name "[]")) t

stringT :: Type
stringT = listT charT

