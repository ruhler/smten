
module Seri.Serif.Polymorphic (
    tvarkind, kindsuf, concrete, tyvars,
    ) where

import qualified Language.Haskell.TH as H

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
tvarkind :: String -> Integer
tvarkind s | s `elem` tyvarsk 0 = 0
tvarkind s | s `elem` tyvarsk 1 = 1
tvarkind s = error $ "no tyvar named " ++ show s

-- Append "" to a string for kind 0
--  and n for kind n
kindsuf :: Integer -> String -> String
kindsuf 0 x = x
kindsuf n x = x ++ show n

-- Given a potentially polymorphic haskell type, convert it to a concrete
-- haskell type which represents the polymorphic seri type.
--
-- In other words, remove all ForallTs and replace all occurences of VarT
-- "foo" with VarT_foo.
concrete :: H.Type -> H.Type
concrete = concrete' []

-- concrete' - same as concrete, but lets you leave a list of variable types
-- unconcrete.
concrete' :: [H.Name] -> H.Type -> H.Type
concrete' ns (H.ForallT _ _ t) = concrete' ns t
concrete' ns t@(H.VarT nm) | nm `elem` ns = t
concrete' ns (H.VarT nm) = H.ConT $ (H.mkName ("VarT_" ++ H.nameBase nm))
concrete' ns (H.AppT a b) = H.AppT (concrete' ns a) (concrete' ns b)
concrete' ns t = t

