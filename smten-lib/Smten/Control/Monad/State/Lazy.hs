
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Control.Monad.State.Lazy (
    State, runState, evalState, execState, mapState, withState,
    StateT, runStateT, evalStateT, execStateT, mapStateT, withStateT,
    liftCatch,
    module Smten.Control.Monad.State.Class,
    module Smten.Control.Monad.Trans,
    )  where

import Smten.Prelude
import Smten.Control.Monad.State.Class
import Smten.Control.Monad.Trans

data State s a = State {
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
    fmap f m = do
        v <- m
        return (f v)

instance Monad (State s) where
    return x = State $ \s -> (x, s)
    (>>=) x f = State $ \s ->   
        let (a, s') = runState x s
        in runState (f a) s'
    fail str = State $ \_ -> error str

instance MonadState s (State s) where
    get = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)


data StateT s m a = StateT {
    runStateT :: s -> m (a, s)
}

evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a

execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT $ runStateT m . f

instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \s -> fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s)
    (>>=) m k = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance (Monad m) => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO x = StateT $ \s -> do
        v <- liftIO x
        return (v, s)

liftCatch :: (m (a,s) -> (e -> m (a,s)) -> m (a,s)) ->
    StateT s m a -> (e -> StateT s m a) -> StateT s m a
liftCatch catchError m h
 = StateT $ \s -> runStateT m s `catchError` \e -> runStateT (h e) s
