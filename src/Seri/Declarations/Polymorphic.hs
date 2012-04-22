
module Seri.Declarations.Polymorphic (
    tyvars, tvarkind
    ) where

import Language.Haskell.TH

-- All the type variables defined we are allowed to use,
-- indexed by kind.
tyvars :: [(Integer, [String])]
tyvars = [(0, ["a", "b", "c", "d"]),
          (1, ["m", "n"])
         ]

tyvarsk :: Integer -> [String]
tyvarsk i = 
  case lookup i tyvars of
     Just xs -> xs
     Nothing -> error $ "No type vars of kind " ++ show i

-- preelem needle haystack
--   Return true if there exists some element in the haystack which is a
--   prefix of the needle.
preelem :: String -> [String] -> Bool
preelem n [] = False
preelem n (x:_) | take (length n) x == n = True
preelem n (_:xs) = preelem n xs

-- Return the kind of a type variable.
--  0 means kind *
--  1 means kind * -> *
--  2 means kind (* -> *) -> * 
--  etc...
tvarkind :: TyVarBndr -> Integer
tvarkind (PlainTV v) | [head (nameBase v)] `preelem` tyvarsk 0 = 0
tvarkind (PlainTV v) | [head (nameBase v)] `preelem` tyvarsk 1 = 1
tvarkind (KindedTV v StarK) = 0
tvarkind (KindedTV v (ArrowK StarK StarK)) = 1
tvarkind v = error $ "TODO: Seri.Declarations.Utils.tvarkind " ++ show v

