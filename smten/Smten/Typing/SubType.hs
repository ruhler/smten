
module Smten.Typing.SubType (
    assignments, isSubType,
    ) where

import Smten.Name
import Smten.Type

-- | assignments poly concrete
-- Given a polymorphic type and a concrete type of the same form, return the
-- mapping from type variable name to concrete type.
assignments :: Type -> Type -> [(Name, Type)]
assignments (VarT n _) t = [(n, t)]
assignments (AppT a b) (AppT a' b') = assignments a a' ++ assignments b b'
assignments _ _ = []

-- | isSubType t sub
-- Return True if 'sub' is a concrete type of 't'.
isSubType :: Type -> Type -> Bool
isSubType t sub =
  let assigns = assignments t sub
      t' = assign assigns t
  in canonical t' == canonical sub
 

