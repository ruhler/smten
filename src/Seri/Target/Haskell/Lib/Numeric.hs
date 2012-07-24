
-- | Library for supporting seri numeric types in the haskell target.
module Seri.Target.Haskell.Lib.Numeric (
    N__(..), N__0(..), N__2p0(..), N__2p1(..),
    N__PLUS(..), N__TIMES(..),
    ) where

class N__ a where
    valueof :: a -> Integer
    numeric :: a

data N__0 = N__0
data N__2p0 n = N__2p0 n
data N__2p1 n = N__2p1 n
data N__PLUS a b = N__PLUS a b
data N__TIMES a b = N__TIMES a b

instance N__ N__0 where
    valueof _ = 0
    numeric = N__0

instance (N__ n) => N__ (N__2p0 n) where
    valueof (N__2p0 n) = 2*(valueof n)
    numeric = N__2p0 numeric

instance (N__ n) => N__ (N__2p1 n) where
    valueof (N__2p1 n) = 2*(valueof n) + 1
    numeric = N__2p1 numeric

instance (N__ a, N__ b) => N__ (N__PLUS a b) where
    valueof (N__PLUS a b) = valueof a + valueof b
    numeric = N__PLUS numeric numeric
    
instance (N__ a, N__ b) => N__ (N__TIMES a b) where
    valueof (N__TIMES a b) = valueof a * valueof b
    numeric = N__TIMES numeric numeric

