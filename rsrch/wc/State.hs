
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module State(
    State, runState, evalState, execState, mapState, withState,
    module Control.Monad.State.Class,
    )  where

import Control.Monad.State.Class
import qualified Map

type S = Map.Map

data State a = State {
    runState :: (S -> (a, S))
}

evalState :: State a -> S -> a
evalState m s = fst (runState m s)

execState :: State a -> S -> S
execState m s = snd (runState m s)

mapState :: ((a, S) -> (b, S)) -> State a -> State b
mapState f m = State $ f . runState m

withState :: (S -> S) -> State a -> State a
withState f m = State $ runState m . f

instance Monad State where
    fail = error
    return x = State $ \s -> (x, s)
    (>>=) x f = State $ \s ->   
        case runState x s of
            (a, s') -> runState (f a) s'
    (>>) x y = x >>= (\_ -> y)

instance MonadState S State where
    get = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)


