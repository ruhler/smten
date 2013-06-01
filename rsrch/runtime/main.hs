
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Prelude as P
import qualified Data.Functor as P
import qualified Data.Maybe as P
import Haskelly

type Char = P.Char

instance Haskelly P.Char Char where
    fromHaskell = P.id
    toHaskell = P.return


type IO = P.IO

instance (Haskelly ha sa) => Haskelly (P.IO ha) (IO sa) where
    fromHaskell x = fromHaskell P.<$> x
    toHaskell x = P.Just ((P.fromMaybe (P.error "toHaskell IO") P.. toHaskell) P.<$> x)

data Unit__ = Unit__

instance Haskelly () Unit__ where
    fromHaskell () = Unit__
    toHaskell Unit__ = P.Just ()

data Maybe a = Nothing | Just a

__caseJust :: Maybe a -> (a -> b) -> b -> b
__caseJust x y n =
    case x of
       Just a -> y a
       _ -> n

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    (>>) a b = a >>= const b

instance Monad IO where
    return = return_io
    (>>=) = bind_io

const :: a -> b -> a
const k = \_ -> k

data Symbolic a

instance Monad Symbolic where
    return = P.error "TODO: return_symbolic"
    (>>=) = P.error "TODO: bind_symbolic"

runSymbolic :: Symbolic Unit__ -> IO (Maybe Unit__)
runSymbolic _ = return (Just Unit__)

-- primitive return_io
return_io :: a -> IO a
return_io = P.return

bind_io :: IO a -> (a -> IO b) -> IO b
bind_io = (P.>>=)


-- primitive putChar
putChar :: Char -> IO Unit__
putChar = fromHaskell P.putChar

-- runtime lib runmain
runmain :: IO Unit__ -> P.IO ()
runmain x = x P.>> P.return ()

main :: P.IO ()
main = runmain P.$
  bind_io (runSymbolic (return Unit__)) P.$ \r ->
    __caseJust r (\_ -> putChar 'Y') (putChar 'N')

