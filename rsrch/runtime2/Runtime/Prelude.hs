
module Runtime.Prelude where

import qualified Prelude as P
import qualified Data.Maybe as P (fromMaybe)
import qualified Data.Functor as P ((<$>))
import Smten.Name

class SmtenHS a where
   -- mux p x y = if p then x else y
   -- Except here p, x, and y may all be symbolic.
   -- You may assume the predicate p is symbolic (you don't have to check for
   -- it being true or false. That's taken care of elsewhere).
   mux :: Bool -> a -> a -> a

   -- Update all variables in the given expression according to the given map.
   -- You may assume all free variables in the expression are in the map.
   realize :: [(Name, Bool)] -> a -> a

instance (SmtenHS a, SmtenHS b) => SmtenHS (a -> b) where
   mux p fa fb = \x -> mux p (fa x) (fb x)
   realize m f = \x -> realize m (f x)

data Bool = False
          | True
          | BoolVar Name
          | BoolMux Bool Bool Bool

instance SmtenHS Bool where
   mux = BoolMux

   realize m False = False
   realize m True = True
   realize m (BoolVar n)
     = P.fromMaybe (P.error "realize name not found") (P.lookup n m)
   realize m (BoolMux p a b)
     = __caseTrue (realize m p) (realize m a) (realize m b)

__caseTrue :: (SmtenHS a) => Bool -> a -> a -> a
__caseTrue p y n =
    case p of
        True -> y
        False -> n
        _ -> mux p y n

data Unit = Unit

instance SmtenHS Unit where
    mux p a b = Unit
    realize m _ = Unit
    
data IO a = 
    IO (P.IO a)
  | IOMux Bool (IO a) (IO a)

instance (SmtenHS a) => SmtenHS (IO a) where
    mux = IOMux
    realize m x = 
        case x of
            IO v -> IO (realize m P.<$> v)
            IOMux p a b -> __caseTrue (realize m p) (realize m a) (realize m b)

data Char = Char P.Char | CharMux Bool Char Char

instance SmtenHS Char where
    mux = CharMux
    realize m x =
        case x of
            Char v -> x
            CharMux p a b -> __caseTrue (realize m p) (realize m a) (realize m b)

putChar :: Char -> IO Unit
putChar (Char c) = IO (P.putChar c P.>> P.return Unit)
putChar (CharMux p a b) = IOMux p (putChar a) (putChar b)
            
