
module Seri.Failable (
    Failable(..), attemptM, onfail,,
    ) where

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

-- | onfail f c
-- Run computation c, if it fails, return the result of calling f on the error
-- message from the failing c.
onfail :: (String -> Failable a) -> Failable a -> Failable a
onfail f (Failable (Left msg)) = f msg
onfail f c = c

