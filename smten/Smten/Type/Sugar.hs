
{-# LANGUAGE PatternGuards #-}

-- Abstract constructors and deconstructors for manipulating smten types.
module Smten.Type.Sugar (
    Type(),
    conT, de_conT,
    appT, de_appT, appsT, de_appsT,
    arrowN, arrowT, de_arrowT, arrowsT, de_arrowsT,

    unitT, boolT, charT, integerT, listT, de_listT, stringT,
    bitT, de_bitT,
    tupleN, de_tupleN, tupleT, de_tupleT,

    addNT, subNT, mulNT,
  ) where

import Control.Monad(guard)
import Data.List(genericLength)

import Smten.Name
import Smten.Type.Type

-- | Form the type constructor with given name.
conT :: Name -> Type
conT = ConT

de_conT :: Type -> Maybe Name
de_conT (ConT n) = Just n
de_conT _ = Nothing

-- | Type application.
-- Given a, b, returns the type (a b)
--
-- Special case: if the result would be an application of
--  ConT "+", ConT "-", or ConT "*" and two numeric types, converts it to the
--  numeric type operation.
--
-- This is a hack to get the haskellf translation work with smtenT and numeric
-- types. The proper solution would be to change the representation of numeric
-- type operations so that #(a+b) is represented as
--   AppT (AppT (ConT "+") a b), or some such similar.
appT :: Type -> Type -> Type
appT (AppT (ConT n) a) b
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

de_listT :: Type -> Maybe Type
de_listT t = do
    (l, v) <- de_appT t
    n <- de_conT l
    guard $ n == name "[]"
    return v

stringT :: Type
stringT = listT charT

bitT :: Integer -> Type
bitT w = appT (conT (name "Bit")) (NumT w)

de_bitT :: Type -> Maybe Integer
de_bitT (AppT (ConT n) w) | n == name "Bit" = Just (nteval w)
de_bitT _ = Nothing

-- Generate the tuple name for given number of arguments.
tupleN :: (Integral n) => n -> Name
tupleN n = name $ "(" ++ replicate (fromIntegral (n-1)) ',' ++ ")"

-- Check if a name is a tuple name. If so, returns the number of elements in
-- the tuple.
de_tupleN :: Name -> Maybe Integer
de_tupleN n = do
    let s = unname n
    guard $ length s > 2
    guard $ head s == '('
    guard $ last s == ')'
    let mid = init (tail s)
    guard $ all (== ',') mid
    return (genericLength mid + 1)

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
    (ConT tn, ts) -> do
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

