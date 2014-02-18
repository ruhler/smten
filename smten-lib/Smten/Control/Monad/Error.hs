
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Control.Monad.Error (
    module Smten.Control.Monad.Error.Class,
    module Smten.Control.Monad,
    module Smten.Control.Monad.Trans,
    ) where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.Trans
import Smten.Control.Monad.Error.Class

instance (Error e) => MonadPlus (Either e) where
    mzero = Left noMsg
    Left _ `mplus` n = n
    m `mplus` _ = m

instance (Error e) => MonadError e (Either e) where
    throwError = Left
    Left l `catchError` h = h l
    Right r `catchError` _ = Right r
