
{-# LANGUAGE PatternGuards #-}

module Smten.Prim.Prim (
    Prim, nullaryP, nullaryTP, unaryP, unaryTP, binaryTP, binaryP,
    ) where

import Control.Monad

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Ppr
import Smten.ExpH

type Prim = Type -> ExpH

nullaryTP :: (SmtenEH a) => String -> (Type -> a) -> Prim
nullaryTP n f = smtenEH . f

nullaryP :: (SmtenEH a) => String -> a -> Type -> ExpH
nullaryP n x = nullaryTP n (const x)

-- | Construct a unary primitve from a haskell function.
unaryTP :: (SmtenEH a, SmtenEH b) => String -> (Type -> a -> b) -> Prim
unaryTP n f =
  let nm = name n

      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_smtenEH a = smtenEH (f t av)
        | IfEH {} <- force a
        , not (smttype (typeof a))
            = strict_appEH t (\a' -> impl t [a']) a
        | ErrorEH s <- force a = errorEH t s
        | otherwise = exph t $ PrimEH nm (impl t) [a]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (_, ot) <- de_arrowT t =
            lamEH t (name "a") $ \a -> impl ot [a]
        | otherwise = error $ "unaryTP.eh type: " ++ pretty t
  in eh

unaryP :: (SmtenEH a, SmtenEH b) => String -> (a -> b) -> Prim
unaryP n f = unaryTP n (const f)

-- | Construct a binary primitive from a haskell function.
binaryTP :: (SmtenEH a, SmtenEH b, SmtenEH c) => String -> (Type -> a -> b -> c) -> Prim
binaryTP n f =
  let nm = name n 

      -- The type is the type of the fully applied primitive
      impl :: Type -> [ExpH] -> ExpH
      impl t [a, b] 
        | Just av <- de_smtenEH a
        , Just bv <- de_smtenEH b = smtenEH (f t av bv)
        | IfEH {} <- force a
        , not (smttype (typeof a))
            = strict_appEH t (\a' -> impl t [a', b]) a
        | IfEH {} <- force b
        , not (smttype (typeof b))
            = strict_appEH t (\b' -> impl t [a, b']) b
        | ErrorEH s <- force b = errorEH t s
        | otherwise = exph t $ PrimEH nm (impl t) [a, b]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (_, bzt) <- de_arrowT t
        , Just (_, ot) <- de_arrowT bzt = 
            lamEH t (name "a") $ \a ->
              lamEH bzt (name "b") $ \b -> impl ot [a, b]
        | otherwise = error $ "binaryTP.eh type: " ++ pretty t
  in eh

binaryP :: (SmtenEH a, SmtenEH b, SmtenEH c) => String -> (a -> b -> c) -> Prim
binaryP n f = binaryTP n (const f)

