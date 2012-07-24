
-- | Library for supporting seri numeric types in the haskell target.
module Seri.Target.Haskell.Lib.Numeric (
    N__(..), N__0(..), N__2p0(..), N__2p1(..),
    N__ADD(..), N__MUL(..),
    ) where

class N__ a where
    valueof :: a -> Integer
    numeric :: a

data N__0 = N__0
data N__2p0 n = N__2p0 n
data N__2p1 n = N__2p1 n

instance N__ N__0 where
    valueof _ = 0
    numeric = N__0

instance (N__ n) => N__ (N__2p0 n) where
    valueof (N__2p0 n) = 2*(valueof n)
    numeric = N__2p0 numeric

instance (N__ n) => N__ (N__2p1 n) where
    valueof (N__2p1 n) = 2*(valueof n) + 1
    numeric = N__2p1 numeric

-- Says a + b = c
class (N__ a, N__ b, N__ c) => N__ADD a b c where {}

-- Says a * b = c
class (N__ a, N__ b, N__ c) => N__MUL a b c where {}
    

