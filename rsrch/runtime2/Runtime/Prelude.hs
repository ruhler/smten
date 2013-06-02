
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language TypeSynonymInstances #-}

module Runtime.Prelude where

import qualified Prelude as P
import qualified Data.Maybe as P (fromMaybe)
import qualified Data.Functor as P ((<$>))
import Smten.Name

import Data.StableMemo.Weak

class SmtenHS a where
   -- mux p x y = if p then x else y
   -- Except here p, x, and y may all be symbolic.
   -- You may assume the predicate p is symbolic (you don't have to check for
   -- it being true or false. That's taken care of elsewhere).
   mux :: Bool -> a -> a -> a

   -- Update all variables in the given expression according to the given map.
   -- You may assume all free variables in the expression are in the map.
   realize :: [(Name, Bool__)] -> a -> a

instance (SmtenHS a, SmtenHS b) => SmtenHS (a -> b) where
   mux p fa fb = \x -> mux p (fa x) (fb x)
   realize m f = \x -> realize m (f x)

data Mux a = Concrete a
           | Mux Bool (Mux a) (Mux a)

instance (SmtenHS a) => SmtenHS (Mux a) where
    mux = Mux

    realize =
      let f m (Concrete a) = Concrete (realize m a)
          f m (Mux p a b) = __caseTrue (realize m p) (realize m a) (realize m b)
      in memo2 f 
        
muxapp :: (SmtenHS b) => (a -> b) -> Mux a -> b
muxapp f =
 let m (Concrete a) = f a
     m (Mux p a b) = mux p (m a) (m b)
 in memo m

mux1app :: (SmtenHS b) => (m a -> b) -> Mux1 m a -> b
mux1app f =
 let m (Concrete1 a) = f a
     m (Mux1 p a b) = mux p (mm a) (mm b)

     mm = memo m
 in mm

data Mux1 m a = Concrete1 (m a)
              | Mux1 Bool (Mux1 m a) (Mux1 m a)

instance (SmtenHS (m a)) => SmtenHS (Mux1 m a) where
    mux = Mux1

    realize =
      let f m (Concrete1 a) = Concrete1 (realize m a)
          f m (Mux1 p a b) = __caseTrue (realize m p) (realize m a) (realize m b)
      in memo2 f 

data Bool__ = False
            | True
            | BoolVar Name

type Bool = Mux Bool__

instance SmtenHS Bool__ where
   mux = P.error "Bool__ mux"

   realize m False = False
   realize m True = True
   realize m (BoolVar n)
     = P.fromMaybe (P.error "realize name not found") (P.lookup n m)

__caseTrue :: (SmtenHS a) => Bool -> a -> a -> a
__caseTrue p y n =
    case p of
        (Concrete True) -> y
        (Concrete False) -> n
        _ -> mux p y n

__mkTrue :: Bool
__mkTrue = Concrete True

__mkFalse :: Bool
__mkFalse = Concrete False

data Unit = Unit

instance SmtenHS Unit where
    mux p a b = Unit
    realize m _ = Unit
    
type IO = Mux1 P.IO

instance (SmtenHS a) => SmtenHS (P.IO a) where
    mux = P.error "Prelude.IO mux"
    realize m x = realize m P.<$> x

type Char = Mux P.Char

instance SmtenHS P.Char where
    mux = P.error "Prelude.Char mux"
    realize m x = x

data Maybe__ a = Nothing | Just a
type Maybe = Mux1 Maybe__

instance (SmtenHS a) => SmtenHS (Maybe__ a) where
    mux = P.error "Maybe__ mux"
    realize m x =
        case x of
            Nothing -> Nothing
            Just v -> Just (realize m v)

__mkJust :: a -> Maybe a
__mkJust = Concrete1 P.. Just

__mkNothing :: Maybe a
__mkNothing = Concrete1 Nothing

__caseJust :: (SmtenHS b) => Maybe a -> (a -> b) -> b -> b
__caseJust x y n = mux1app (\x' ->
    case x' of
        Just v -> y v
        Nothing -> n) x

class Monad m where
    return :: a -> m a
    (>>=) :: (SmtenHS b) => m a -> (a -> m b) -> m b
    (>>) :: (SmtenHS b) => m a -> m b -> m b
    (>>) a b = a >>= const b
            
instance Monad IO where
    return = return_io
    (>>=) = bind_io

const :: a -> b -> a
const k = \_ -> k

-- primitive return_io
return_io :: a -> IO a
return_io x = Concrete1 (P.return x)

bind_io :: (SmtenHS b) => IO a -> (a -> IO b) -> IO b
bind_io m f = mux1app (\mx ->
    let g x = let Concrete1 z = f x in z
    in Concrete1 (mx P.>>= g)) m

-- primitive putChar
putChar :: Char -> IO Unit
putChar = muxapp P.$ \c -> Concrete1 (P.putChar c P.>> P.return Unit)

not :: Bool -> Bool
not p = __caseTrue p __mkFalse __mkTrue

(&&) :: Bool -> Bool -> Bool
(&&) x y = __caseTrue x y __mkFalse

(||) :: Bool -> Bool -> Bool
(||) x y = __caseTrue x __mkTrue y

