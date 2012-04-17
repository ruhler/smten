
module Seri.InstId (
    InstId(..), idize,
    ) where

import Language.Haskell.TH

data InstId = NoInst | InstId String
    deriving(Eq, Show)

-- Given the type for an instance delcaration, return a unique string
-- identifying that instance.
--
-- The return string is suitable for use as part of the name of an identifier
-- in haskell via template haskell.
idize :: Type -> String
idize (AppT a b) = idize a ++ "$" ++ idize b
idize (ConT nm) = nameBase nm

