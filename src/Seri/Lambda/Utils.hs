
module Seri.Lambda.Utils (
    arrowsT, outputT,
    ) where

import Seri.Lambda.IR

-- Given the list [a, b, ..., c]
-- Return the type (a -> b -> ... -> c)
arrowsT :: [Type] -> Type
arrowsT [t] = t
arrowsT (t:ts) = AppT (AppT (ConT "->") t) (arrowsT ts)

-- Given a type of the from (a -> b)
-- Returns b
outputT :: Type -> Type
outputT (AppT (AppT (ConT "->") _) t) = t
outputT t = t

