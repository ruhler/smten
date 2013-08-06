
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Control.Monad.Error.Class (
    Error(..), MonadError(..),
    )  where

import Smten.Prelude
import Smten.Control.Monad.State.Lazy as LazyState

class Error a where
    noMsg :: a
    noMsg = strMsg ""

    strMsg :: String -> a
    strMsg _ = noMsg

instance Error String where
    noMsg = ""
    strMsg = id

class (Monad m) => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance (MonadError e m) => MonadError e (LazyState.StateT s m) where
    throwError = lift . throwError
    catchError = LazyState.liftCatch catchError

