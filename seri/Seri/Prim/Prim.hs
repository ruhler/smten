
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

      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_seriEH a = seriEH (f t av)
        | Just (_, msg) <- de_errorEH a =
            let Just (_, ot) = de_arrowT t
            in errorEH ot msg
        | CaseEH {} <- a
        , not (smttype (typeof a)) =
            -- | f (case x of { k -> y ; _ -> n})
            -- ==> case x of { k -> f y ; _ -> f n }
            let Just (_, ot) = de_arrowT t
                g = lamEH (Sig (name "_x") (typeof a)) ot $ \a' -> impl t [a']
            in pushfun g a
        | otherwise =
            let Just (_, ot) = de_arrowT t
            in PrimEH nm ot (impl t) [a]

      eh :: Type -> ExpH
      eh t
        | Just (at, ot) <- de_arrowT t =
            lamEH (Sig (name "a") at) ot $ \a ->
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
        | Just (_, msg) <- mplus (de_errorEH a) (de_errorEH b) =
            let Just (_, bot) = de_arrowT t
                Just (_, ot) = de_arrowT bot
            in errorEH ot msg
        | CaseEH {} <- a
        , not (smttype (typeof a)) =
            -- | f (case x of { k -> y ; _ -> n}) b
            -- ==> case x of { k -> f y b ; _ -> f n b } 
            let Just (_, bot) = de_arrowT t
                Just (_, ot) = de_arrowT bot
                g = lamEH (Sig (name "_x") (typeof a)) ot $ \a' -> impl t [a', b]
            in pushfun g a
        | CaseEH {} <- b
        , not (smttype (typeof b)) =
            -- | f a (case x of { k -> y ; _ -> n})
            -- ==> case x of { k -> f a y ; _ -> f a n } 
            let Just (_, bot) = de_arrowT t
                Just (_, ot) = de_arrowT bot
                g = lamEH (Sig (name "_x") (typeof b)) ot $ \b' -> impl t [a, b']
            in pushfun g b
        | otherwise =
            let Just (_, bot) = de_arrowT t
                Just (_, ot) = de_arrowT bot
            in PrimEH nm ot (impl t) [a, b]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (at, bzt) <- de_arrowT t
        , Just (bt, ot) <- de_arrowT bzt = 
            lamEH (Sig (name "a") at) bzt $ \a ->
              lamEH (Sig (name "b") bt) ot $ \b ->
                impl t [a, b]
        | otherwise = error $ "binaryTP.eh type: " ++ pretty t
  in Prim nm eh

binaryP :: (SeriEH a, SeriEH b, SeriEH c) => String -> (a -> b -> c) -> Prim
binaryP n f = binaryTP n (const f)

