
{-# LANGUAGE PatternGuards #-}

module Seri.Prim.Prim (
    Prim(),
    primEH, lookupPrim,
    nullaryP, nullaryTP, unaryP, unaryTP, binaryTP, binaryP,
    ) where

import Control.Monad

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

      -- Type is the type of the fully applied primitive.
      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_seriEH a = seriEH (f t av)
        | Just (_, msg) <- de_errorEH a = errorEH t msg
        | CaseEH {} <- a
        , not (smttype (typeof a)) =
            -- | f (case x of { k -> y ; _ -> n})
            -- ==> case x of { k -> f y ; _ -> f n }
            let g = lamEH (Sig (name "_x") (typeof a)) t $ \a' -> impl t [a']
            in pushfun g a
        | otherwise = identify $ \id -> PrimEH id nm t (impl t) [a]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (at, ot) <- de_arrowT t =
            lamEH (Sig (name "a") at) ot $ \a ->
              impl ot [a]
        | otherwise = error $ "unaryTP.eh type: " ++ pretty t
  in Prim nm eh

unaryP :: (SeriEH a, SeriEH b) => String -> (a -> b) -> Prim
unaryP n f = unaryTP n (const f)

-- | Construct a binary primitive from a haskell function.
binaryTP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (Type -> a -> b -> c) -> Prim
binaryTP n f =
  let nm = name n 

      -- The type is the type of the fully applied primitive
      impl :: Type -> [ExpH] -> ExpH
      impl t [a, b] 
        | Just av <- de_seriEH a
        , Just bv <- de_seriEH b = seriEH (f t av bv)
        | Just (_, msg) <- mplus (de_errorEH a) (de_errorEH b) = errorEH t msg
        | CaseEH {} <- a
        , not (smttype (typeof a)) =
            -- | f (case x of { k -> y ; _ -> n}) b
            -- ==> case x of { k -> f y b ; _ -> f n b } 
            let g = lamEH (Sig (name "_x") (typeof a)) t $ \a' -> impl t [a', b]
            in pushfun g a
        | CaseEH {} <- b
        , not (smttype (typeof b)) =
            -- | f a (case x of { k -> y ; _ -> n})
            -- ==> case x of { k -> f a y ; _ -> f a n } 
            let g = lamEH (Sig (name "_x") (typeof b)) t $ \b' -> impl t [a, b']
            in pushfun g b
        | otherwise = identify $ \id -> PrimEH id nm t (impl t) [a, b]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (at, bzt) <- de_arrowT t
        , Just (bt, ot) <- de_arrowT bzt = 
            lamEH (Sig (name "a") at) bzt $ \a ->
              lamEH (Sig (name "b") bt) ot $ \b ->
                impl ot [a, b]
        | otherwise = error $ "binaryTP.eh type: " ++ pretty t
  in Prim nm eh

binaryP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (a -> b -> c) -> Prim
binaryP n f = binaryTP n (const f)

