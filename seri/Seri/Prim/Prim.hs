
{-# LANGUAGE PatternGuards #-}

module Seri.Prim.Prim (
    Prim(),
    primEH, lookupPrim,
    unaryP, binaryP,
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

-- | Construct a unary primitve from a haskell function.
unaryP :: (SeriEH a, SeriEH b) => String -> (a -> b) -> Prim
unaryP n f =
  let nm = name n

      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_seriEH a = seriEH (f av)
        | otherwise = PrimEH (Sig nm t) (impl t) [a]

      eh :: Type -> ExpH
      eh t
        | Just (at, _) <- de_arrowT t =
            lamEH (Sig (name "a") at) $ \a ->
              impl t [a]
        | otherwise = error $ "unaryP.eh type: " ++ pretty t
  in Prim nm eh

-- | Construct a binary primitive from a haskell function.
binaryP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (a -> b -> c) -> Prim
binaryP n f =
  let nm = name n 

      -- The type is the type of the primitive function without arguments
      -- applied.
      impl :: Type -> [ExpH] -> ExpH
      impl t [a, b] 
        | Just av <- de_seriEH a
        , Just bv <- de_seriEH b = seriEH (f av bv)
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
        | otherwise = error $ "binaryP.eh type: " ++ pretty t
  in Prim nm eh

