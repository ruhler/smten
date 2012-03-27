{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Seri.TypeCheck (
    TypeCheck(..),
    typefail, typeassert,
    ) where

import Seri.Name

class TypeCheck t e | e -> t where
    -- Return the type of an expression.
    typeof :: e -> t

    -- Check that all variables with the given name in the expression have the
    -- given type. If they don't, fail with a hopefully meaningful message.
    checkvars :: Monad m => Name -> t -> e -> m ()


-- typefail expected found expr
-- Indicate a type failure.
typefail :: (Monad m, Show t, Show e) => String -> t -> e -> m a
typefail exp fnd expr
  = fail $ "Expected type " ++ exp ++ ", found type " ++ show fnd
            ++ " in the expression " ++ show expr

typeassert :: (Eq t, Show t, Show e) => (Monad m) => t -> t -> e -> m ()
typeassert exp fnd expr
  = if (exp == fnd)
        then return ()
        else typefail (show exp) fnd expr
  
