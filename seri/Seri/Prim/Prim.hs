
{-# LANGUAGE PatternGuards #-}

module Seri.Prim.Prim (
    Prim(),
    primEH, lookupPrim,
    nullaryP, nullaryTP, unaryP, unaryTP, binaryTP, binaryP,
    ) where

import qualified Seri.HashTable as HT
import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Ppr
import Seri.ExpH


data Prim = Prim {
    p_name :: Name,
    primEH :: Type -> ExpH
}

lookupPrim :: [Prim] -> Sig -> Maybe ExpH
lookupPrim ps =
  let m = HT.table [(n, f) | Prim n f <- ps]
  in \(Sig n t) -> do
    f <- HT.lookup n m
    return (f t)

nullaryTP :: (SeriEH a) => String -> (Type -> a) -> Prim
nullaryTP n f = Prim (name n) (seriEH . f)

nullaryP :: (SeriEH a) => String -> a -> Prim
nullaryP n x = nullaryTP n (const x)

-- | Construct a unary primitve from a haskell function.
unaryTP :: (SeriEH a, SeriEH b) => String -> (Type -> a -> b) -> Prim
unaryTP n f =
  let nm = name n

      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_seriEH a = seriEH (f t av)
        | otherwise = PrimEH (Sig nm t) (impl t) [a]

      eh :: Type -> ExpH
      eh t
        | Just (at, _) <- de_arrowT t =
            lamEH (Sig (name "a") at) $ \a ->
              impl t [a]
        | otherwise = error $ "unaryTP.eh type: " ++ pretty t
  in Prim nm eh

unaryP :: (SeriEH a, SeriEH b) => String -> (a -> b) -> Prim
unaryP n f = unaryTP n (const f)

-- | Construct a binary primitive from a haskell function.
binaryTP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (Type -> a -> b -> c) -> Prim
binaryTP n f =
  let nm = name n 

      -- The type is the type of the primitive function without arguments
      -- applied.
      impl :: Type -> [ExpH] -> ExpH
      impl t [a, b] 
        | Just av <- de_seriEH a
        , Just bv <- de_seriEH b = seriEH (f t av bv)
        | otherwise = PrimEH (Sig nm t) (impl t) [a, b]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (at, bzt) <- de_arrowT t
        , Just (bt, _) <- de_arrowT bzt = 
            lamEH (Sig (name "a") at) $ \a ->
              lamEH (Sig (name "b") bt) $ \b ->
                impl t [a, b]
        | otherwise = error $ "binaryTP.eh type: " ++ pretty t
  in Prim nm eh

binaryP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (a -> b -> c) -> Prim
binaryP n f = binaryTP n (const f)

