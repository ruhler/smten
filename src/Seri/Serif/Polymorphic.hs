
module Seri.Serif.Polymorphic (
    tvarkind, kindsuf
    ) where

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

-- Append "" to a string for kind 0
--  and n for kind n
kindsuf :: Integer -> String -> String
kindsuf 0 x = x
kindsuf n x = x ++ show n

