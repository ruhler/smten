
module Seri.Builtin (
    Builtin(..), builtins,
 ) where

import Seri.Ppr

data Builtin = Builtin {
    -- optionally return a target name for the given primitive which is
    -- different from the primitive name.
    mapprim :: String -> Maybe String,

    -- optionally return a target name for the given type constructor which is
    -- different from the given type constructor name.
    maptype :: String -> Maybe String,

    -- Additional includes or imports to include in the target output code.
    includes :: Doc
}

-- Compose builtins together.
builtins :: Builtin -> Builtin -> Builtin
builtins a b =
    let mergelookup :: (String -> Maybe String) -> (String -> Maybe String)
                        -> (String -> Maybe String)
        mergelookup a b str = 
            case (a str, b str) of
                (Just x, Nothing) -> Just x
                (Nothing, Just x) -> Just x
                (Nothing, Nothing) -> Nothing
                (Just x, Just y) -> error $ "multiply defined builtin: " ++ x
    in Builtin {
         mapprim = mergelookup (mapprim a) (mapprim b),
         maptype = mergelookup (maptype a) (maptype b),
         includes = includes a $+$ includes b
       }

