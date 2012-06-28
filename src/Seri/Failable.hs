
-- | A monad for dealing with computations which can fail.
module Seri.Failable (
    Failable(..), attemptM, attemptIO, surely, onfail, (<|>),
    ) where

import System.IO
import System.Exit

data Failable a = Failable {
    attempt :: Either String a
}

instance Monad Failable where
    return x = Failable $ Right x
    fail msg = Failable $ Left msg
    (>>=) (Failable x) f = Failable $ 
        case x of
            Left msg -> Left msg
            Right a -> attempt (f a)

-- | Attempt a failable computation in a Monad.
-- fails in the monad if failable fails.
attemptM :: (Monad m) => Failable a -> m a
attemptM (Failable (Left msg)) = fail msg
attemptM (Failable (Right a)) = return a

-- | Attempt a failable computation in IO.
-- Prints the error message and exits failure on failure.
attemptIO :: Failable a -> IO a
attemptIO (Failable (Left msg)) = do
    hPutStrLn stderr msg
    exitFailure
attemptIO (Failable (Right a)) = return a

-- | Return the result of a failable computation sure to complete.
-- It's an error if the computation fails.
surely :: Failable a -> a
surely (Failable (Right a)) = a
surely (Failable (Left msg)) = error msg
    

-- | onfail f c
-- Run computation c, if it fails, return the result of calling f on the error
-- message from the failing c.
onfail :: (String -> Failable a) -> Failable a -> Failable a
onfail f (Failable (Left msg)) = f msg
onfail f c = c

-- | a <|> b
--   Return the result of 'a' if it succeeds, otherwise the result of 'b'.
(<|>) :: Failable a -> Failable a -> Failable a
(<|>) a b =
    case attemptM a of
       Just x -> return x
       Nothing -> b

