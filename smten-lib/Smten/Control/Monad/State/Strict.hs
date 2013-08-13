
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
-- | The implementation of this module is based on 
-- the source for Control.Monad.State.Strict from mtl-1.1.1.1
module Smten.Control.Monad.State.Strict (
    module Smten.Control.Monad.State.Class,
    -- * The State Monad
    State(..),
    evalState,
    execState,
    mapState,
    withState,
    -- * The StateT Monad
    StateT(..),
    evalStateT,
    execStateT,
    mapStateT,
    withStateT,
    module Smten.Control.Monad,
    module Smten.Control.Monad.Trans,
    )  where

import Smten.Prelude
import Smten.Control.Monad
import Smten.Control.Monad.Error.Class
import Smten.Control.Monad.State.Class
import Smten.Control.Monad.Trans

newtype State s a = State {
    runState :: (s -> (a, s))
}

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = State $ f . runState m

withState :: (s -> s) -> State s a -> State s a
withState f m = State $ runState m . f

instance Functor (State s) where
    fmap f m = State $ \s -> case runState m s of
                                (a, s') -> (f a, s')

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (>>=) x f = State $ \s ->   
        case runState x s of
            (a, s') -> runState (f a) s'

instance MonadState s (State s) where
    get = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)


newtype StateT s m a = StateT {
    runStateT :: s -> m (a, s)
}

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (_, s') <- runStateT m s
    return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Monad m) => Functor (StateT s m) where
    fmap f m = StateT $ \s -> do
        (x, s') <- runStateT m s
        return (f x, s')

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    (>>=) m k = StateT $ \s -> do
        (a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero = StateT $ \_ -> mzero
    m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance (Monad m) => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance (MonadError e m) => MonadError e (StateT s m) where
    throwError = lift . throwError
    m `catchError` h = StateT $ \s -> runStateT m s
        `catchError` \e -> runStateT (h e) s

