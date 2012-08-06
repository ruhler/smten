
-- | Library for supporting bit vectors in the haskell target.
module Seri.Target.Haskell.Lib.Bit(
    Bit()
    ) where

import qualified Seri.Bit as B
import Seri.Target.Haskell.Lib.Numeric

data Bit n = Bit n B.Bit

instance (N__ n) => Eq (Bit n) where
    (==) (Bit _ a) (Bit _ b) = a == b

instance (N__ n) => Num (Bit n) where
    (+) (Bit w a) (Bit _ b) = Bit w (a+b)
    (*) (Bit w a) (Bit _ b) = Bit w (a*b)
    (-) (Bit w a) (Bit _ b) = Bit w (a-b)
    fromInteger i = 
        let w = numeric 
        in Bit w (B.mkBit (valueof w) i)
    abs = error "todo: abs Bit"
    signum = error "todo: signum Bit"
    
