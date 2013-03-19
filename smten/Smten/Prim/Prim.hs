
{-# LANGUAGE PatternGuards #-}

module Smten.Prim.Prim (
    Prim(), PrimF(..),
    primEH, lookupPrim,
    nullaryP, nullaryTP, unaryP, unaryTP, unaryPF, binaryTP, binaryP, binaryPF,
    ) where

import Control.Monad

import qualified Smten.HashTable as HT
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Ppr
import Smten.ExpH

data PrimF a = PrimF {
    p_prim :: Prim,
    p_impl :: a
}

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

nullaryTP :: (SmtenEH a) => String -> (Type -> a) -> Prim
nullaryTP n f = Prim (name n) (smtenEH . f)

nullaryP :: (SmtenEH a) => String -> a -> Prim
nullaryP n x = nullaryTP n (const x)

-- | Construct a unary primitve from a haskell function.
unaryTP :: (SmtenEH a, SmtenEH b) => String -> (Type -> a -> b) -> Prim
unaryTP n f =
  let nm = name n

      -- Type is the type of the fully applied primitive.
      impl :: Type -> [ExpH] -> ExpH
      impl t [a]
        | Just av <- de_smtenEH a = smtenEH (f t av)
        | IfEH {} <- force a
        , not (smttype (typeof (force a)))
            = strict_appEH t (\a' -> impl t [a']) a
        | otherwise = thunk $ PrimEH nm t (impl t) [a]

      -- The type is the type of the primitive function without arguments
      -- applied.
      eh :: Type -> ExpH
      eh t
        | Just (at, ot) <- de_arrowT t =
            lamEH (Sig (name "a") at) ot $ \a ->
              impl ot [a]
        | otherwise = error $ "unaryTP.eh type: " ++ pretty t
  in Prim nm eh

unaryP :: (SmtenEH a, SmtenEH b) => String -> (a -> b) -> Prim
unaryP n f = unaryTP n (const f)

unaryPF :: (SmtenEH a, SmtenEH b) => String -> (a -> b) -> PrimF (a -> b)
unaryPF n f = PrimF (unaryP n f) f

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
        , not (smttype (typeof (force a)))
            = strict_appEH t (\a' -> impl t [a', b]) a
        | IfEH {} <- force b
        , not (smttype (typeof (force b)))
            = strict_appEH t (\b' -> impl t [a, b']) b
        | otherwise = thunk $ PrimEH nm t (impl t) [a, b]

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

binaryP :: (SmtenEH a, SmtenEH b, SmtenEH c) => String -> (a -> b -> c) -> Prim
binaryP n f = binaryTP n (const f)

binaryPF :: (SmtenEH a, SmtenEH b, SmtenEH c) => String -> (a -> b -> c) -> PrimF (a -> b -> c)
binaryPF n f = PrimF (binaryP n f) f

