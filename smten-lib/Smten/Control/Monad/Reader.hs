
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Control.Monad.Reader (
    module Smten.Control.Monad.Reader.Class,
    Reader(..),
    mapReader,
    withReader,
    ReaderT(..),
    mapReaderT,
    withReaderT,
    module Smten.Control.Monad,
    module Smten.Control.Monad.Trans,
    ) where

import Smten.Data.Function
import Smten.Data.Functor
import Smten.Control.Monad
import Smten.Control.Monad.Reader.Class
import Smten.Control.Monad.Trans

newtype Reader r a = Reader {
    runReader :: r -> a
}

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f m = Reader $ f . runReader m

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader f m = Reader $ runReader m . f

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

instance MonadReader r (Reader r) where
    ask = Reader id
    local f m = Reader $ runReader m . f

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do   
        a <- runReaderT m r
        return (f a)

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \_ -> return a
    m >>= k = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT $ \_ -> fail msg

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local f m = ReaderT $ \r -> runReaderT m (f r)

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

