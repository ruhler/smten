
module Seri.Declarations.Polymorphic (
    tyvars, tvarkind, tvnamekind, concrete, concrete',
    ) where

import Language.Haskell.TH
import Seri.THUtils

-- All the type variables defined that we are allowed to use,
-- indexed by kind.
tyvars :: [(Integer, [String])]
tyvars = [(0, ["a", "b", "c", "d"]),
          (1, ["m", "n"])
         ]

-- Return a list of the available type variables for the given kind.
tyvarsk :: Integer -> [String]
tyvarsk i = 
  case lookup i tyvars of
     Just xs -> xs
     Nothing -> error $ "No type vars of kind " ++ show i

-- Return the kind of a type variable based on its name
tvnamekind :: String -> Integer
tvnamekind s | s `elem` tyvarsk 0 = 0
tvnamekind s | s `elem` tyvarsk 1 = 1

-- Return the kind of a type variable.
--  0 means kind *
--  1 means kind * -> *
--  2 means kind * -> (* -> *)
--  etc...
tvarkind :: TyVarBndr -> Integer
tvarkind = tvnamekind . nameBase . tyvarname

-- Given a potentially polymorphic haskell type, convert it to a concrete
-- haskell type which represents the polymorphic seri type.
--
-- In other words, remove all ForallTs and replace all occurences of VarT
-- "foo" with VarT_foo.
concrete :: Type -> Type
concrete = concrete' []

-- concrete' - same as concrete, but lets you leave a list of variable types
-- unconcrete.
concrete' :: [Name] -> Type -> Type
concrete' ns (ForallT _ _ t) = concrete' ns t
concrete' ns t@(VarT nm) | nm `elem` ns = t
concrete' ns (VarT nm) = ConT $ prefixed "VarT_" nm
concrete' ns (AppT a b) = AppT (concrete' ns a) (concrete' ns b)
concrete' ns t = t

