
module Seri.Lambda.Utils (
    appsT, arrowsT, outputT, listT,
    ) where

import Seri.Lambda.IR

-- Give the list [a, b, ..., c]
-- Return the type (a b ... c)
appsT :: [Type] -> Type
appsT = foldl1 AppT 

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

-- Given a type a, returns the type [a]
listT :: Type -> Type
listT t = AppT (ConT "[]") t

