
module Seri.Target.Haskell.Lib.Integer (
    Prelude.Integer, (+), (-), (*), (<), (>), (==)
    ) where

import qualified Prelude
import Prelude (Integer, Bool)

(+) :: Integer -> Integer -> Integer
(+) = (Prelude.+)

(-) :: Integer -> Integer -> Integer
(-) = (Prelude.-)

(*) :: Integer -> Integer -> Integer
(*) = (Prelude.*)

(<) :: Integer -> Integer -> Bool
(<) = (Prelude.<)

(>) :: Integer -> Integer -> Bool
(>) = (Prelude.>)

(==) :: Integer -> Integer -> Bool
(==) = (Prelude.==)

