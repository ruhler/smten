
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Smten.Runtime.SmtenHS (
    SmtenHS0(..), SmtenHS1(..), SmtenHS2(..), SmtenHS3(..), SmtenHS4(..),
    ite, iterealize, realize, flrealize, flmerge, flsapp, primsapp,
    ) where

import Prelude hiding (Bool(..), Integer)
import Data.Maybe

import Smten.Runtime.Types

class SmtenHS0 a where
    error0 :: ErrorString -> a
    ite0 :: Bool -> a -> a -> a
    realize0 :: Model -> a -> a
    primitive0 :: (Model -> a) -> a -> a
    primitive0 = const id

class SmtenHS1 m where
    error1 :: (SmtenHS0 a) => ErrorString -> m a
    ite1 :: (SmtenHS0 a) => Bool -> m a -> m a -> m a
    realize1 :: (SmtenHS0 a) => Model -> m a -> m a
    primitive1 :: (SmtenHS0 a) => (Model -> m a) -> m a -> m a
    primitive1 = const id

instance (SmtenHS0 a, SmtenHS1 m) => SmtenHS0 (m a) where
    error0 = error1
    ite0 = ite1
    realize0 = realize1
    primitive0 = primitive1

class SmtenHS2 m where
    error2 :: (SmtenHS0 a, SmtenHS0 b) => ErrorString -> m a b
    ite2 :: (SmtenHS0 a, SmtenHS0 b) => Bool -> m a b -> m a b -> m a b
    realize2 :: (SmtenHS0 a, SmtenHS0 b) => Model -> m a b -> m a b
    primitive2 :: (SmtenHS0 a, SmtenHS0 b) => (Model -> m a b) -> m a b -> m a b
    primitive2 = const id

instance (SmtenHS0 a, SmtenHS2 m) => SmtenHS1 (m a) where
    error1 = error2
    ite1 = ite2
    realize1 = realize2
    primitive1 = primitive2

class SmtenHS3 m where
    error3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => ErrorString -> m a b c
    ite3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => Bool -> m a b c -> m a b c -> m a b c
    realize3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => Model -> m a b c -> m a b c
    primitive3 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c) => (Model -> m a b c) -> m a b c -> m a b c
    primitive3 = const id

instance (SmtenHS0 a, SmtenHS3 m) => SmtenHS2 (m a) where
    error2 = error3
    ite2 = ite3
    realize2 = realize3
    primitive2 = primitive3

class SmtenHS4 m where
    error4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => ErrorString -> m a b c d
    ite4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => Bool -> m a b c d -> m a b c d -> m a b c d
    realize4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => Model -> m a b c d -> m a b c d
    primitive4 :: (SmtenHS0 a, SmtenHS0 b, SmtenHS0 c, SmtenHS0 d) => (Model -> m a b c d) -> m a b c d -> m a b c d
    primitive4 = const id


instance (SmtenHS0 a, SmtenHS4 m) => SmtenHS3 (m a) where
    error3 = error4
    ite3 = ite4
    realize3 = realize4
    primitive3 = primitive4

{-# INLINEABLE ite #-}
ite :: (SmtenHS0 a) => Bool -> a -> a -> a
ite True a _ = a
ite False _ b = b
ite (Bool_Not x) a b = ite x b a
ite p a b = ite0 p a b

{-# INLINEABLE iterealize #-}
iterealize :: (SmtenHS0 a) => Bool -> a -> a -> Model -> a
iterealize p a b m = ite (realize m p) (realize m a) (realize m b)
    
realize :: (SmtenHS0 a) => Model -> a -> a
realize m x = m_cached m realize0 x

flrealize :: (SmtenHS0 a) => Model -> [Maybe (Bool, a)] -> a
flrealize m (Nothing : xs) = flrealize m xs
flrealize m (Just (p, v) : xs) = ite (realize m p) (realize m v) (flrealize m xs)
flrealize _ [] = error "flrealize failed"

-- flmerge
-- Merge two fields of an ite constructor.
{-# INLINEABLE flmerge #-}
flmerge :: (SmtenHS0 a) => Bool -> Maybe (Bool, a) -> Maybe (Bool, a) -> Maybe (Bool, a)
flmerge _ Nothing Nothing = Nothing
flmerge p (Just (g, v)) Nothing = Just (andB p g, v)
flmerge p Nothing (Just (g, v)) = Just (andB (notB p) g, v)
flmerge p (Just (ga, va)) (Just (gb, vb)) = Just (ite p ga gb, ite p va vb)

{-# INLINEABLE flsapp #-}
flsapp :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> a -> [Maybe (Bool, a)] -> b
flsapp f x zs =
 let join [Just (_, v)] = f v
     join (Just (p, a):bs) = ite p (f a) (join bs)
     zs' = filter isJust zs
 in primitive0 (\m -> realize m (f (realize m x))) (join zs')

{-# INLINEABLE primsapp #-}
primsapp :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> (Model -> a) -> a -> b
primsapp f r c = primitive0 (\m -> realize m (f (r m))) (f c)

instance SmtenHS0 Bool where
    error0 = Bool_Err
    ite0 = iteB
    realize0 m x =
      case x of
        True -> True
        False -> False
        Bool_Ite p a b -> iterealize p a b m
        Bool_And a b -> andB (realize m a) (realize m b)
        Bool_Not p -> notB (realize m p)
        Bool_EqInteger a b -> eq_Integer (realize m a) (realize m b)
        Bool_LeqInteger a b -> leq_Integer (realize m a) (realize m b)
        Bool_EqBit a b -> eq_Bit (realize m a) (realize m b)
        Bool_LeqBit a b -> leq_Bit (realize m a) (realize m b)
        Bool_Var n -> lookupBool m n
        Bool_Err msg -> Bool_Err (realize m msg)
        Bool_Prim r _ -> r m

instance SmtenHS0 Integer where
   error0 = Integer_Err
   ite0 = Integer_Ite
   realize0 m x = 
      case x of
         Integer {} -> x
         Integer_Add a b -> add_Integer (realize m a) (realize m b)
         Integer_Sub a b -> sub_Integer (realize m a) (realize m b)
         Integer_Ite p a b -> iterealize p a b m
         Integer_Var n -> lookupInteger m n
         Integer_Err msg -> Integer_Err (realize m msg)
         Integer_Prim r _ -> r m
   
instance SmtenHS0 (Bit n) where
    error0 = Bit_Err
    realize0 m x = 
      case x of
        Bit {} -> x
        Bit_Add a b -> add_Bit (realize m a) (realize m b)
        Bit_Sub a b -> sub_Bit (realize m a) (realize m b)
        Bit_Mul a b -> mul_Bit (realize m a) (realize m b)
        Bit_Or a b -> or_Bit (realize m a) (realize m b)
        Bit_And a b -> and_Bit (realize m a) (realize m b)
        Bit_Shl a b -> shl_Bit (realize m a) (realize m b)
        Bit_Lshr a b -> lshr_Bit (realize m a) (realize m b)
        Bit_Concat a b -> concat_Bit (realize m a) (realize m b)
        Bit_Not a -> not_Bit (realize m a)
        Bit_SignExtend by x -> sign_extend_Bit by (realize m x)
        Bit_Extract hi lo x -> extract_Bit hi lo (realize m x)
        Bit_Ite p a b -> iterealize p a b m
        Bit_Var n -> lookupBit m n
        Bit_Err msg -> Bit_Err (realize m msg)
        Bit_Prim r _ -> r m
    ite0 = Bit_Ite
    primitive0 = Bit_Prim

instance SmtenHS0 ErrorString where
   realize0 m x@(ErrorString str) = x
   realize0 m (ErrorString_Ite p a b) = iterealize p a b m
   error0 = id
   ite0 = ErrorString_Ite

instance SmtenHS2 (->) where
    error2 msg = \x -> error0 msg
    realize2 m f = \x -> realize m (f (realize m x))
    ite2 p fa fb = \x -> ite p (fa x) (fb x)
    primitive2 r f = \x -> primitive0 (\m -> r m x) (f x)

