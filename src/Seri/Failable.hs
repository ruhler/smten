
module Seri.Failable (
    Failable(..), attemptM,
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

attemptM :: (Monad m) => Failable a -> m a
attemptM (Failable (Left msg)) = fail msg
attemptM (Failable (Right a)) = return a

