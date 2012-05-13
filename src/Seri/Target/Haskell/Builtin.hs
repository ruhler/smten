
module Seri.Target.Haskell.Builtin (
    Builtin(..), builtins,
 ) where

import qualified Language.Haskell.TH as H
import Seri
import Seri.Ppr

data Builtin = Builtin {
    -- Optionally give a builtin implementation for the given seri expression.
    mapexp :: Exp -> Maybe H.Exp,

    -- Optionally give a builtin implementation for the given seri type.
    maptype :: Type -> Maybe H.Type,

    -- Additional includes or imports to include in the target output code.
    includes :: Doc
}

-- Compose builtins together.
builtins :: [Builtin] -> Builtin
builtins bs =
    let mergelookup :: (Show a) => (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
        mergelookup a b k = 
            case (a k, b k) of
                (Just x, Nothing) -> Just x
                (Nothing, Just x) -> Just x
                (Nothing, Nothing) -> Nothing
                (Just x, Just y) -> error $ "multiply defined builtin for " ++ show k

        compose :: Builtin -> Builtin -> Builtin
        compose a b = Builtin {
             mapexp = mergelookup (mapexp a) (mapexp b),
             maptype = mergelookup (maptype a) (maptype b),
             includes = includes a $+$ includes b
           }
    in foldl1 compose bs

