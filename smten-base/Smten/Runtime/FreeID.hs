
module Smten.Runtime.FreeID (
    FreeID, freenm,
    Fresh, runFresh, fresh,
    withfresh,
    ) where

import Data.IORef
import System.IO.Unsafe

type FreeID = Integer

-- TODO: Maybe we could use a more compact representation, like base 52, to
-- make shorter variable names? Would it be prettier? More efficient?
freenm :: FreeID -> String
freenm x = {-# SCC "FreeName" #-} "f~" ++ show x

-- A FreePool represents the (infinite) set of FreeIDs:
--    [fp_base + i*fp_incr | i <- [0..]]
data FreePool = FreePool {
    fp_base :: Integer,
    fp_incr :: Integer
}

newtype Fresh a = Fresh {
    runFresh_ :: FreePool -> a
}

-- | A Monad for allocating Fresh ids.
-- The Monad allocates ids lazily in the sense that they are allocated on
-- demand in any order, and do not impose a constraint on the order
-- computation happens in.
instance Monad Fresh where
    return x = Fresh (const x)
    (>>=) x f = Fresh (\p ->
        let p1 = p { fp_incr = 2 * fp_incr p }
            p2 = p1 { fp_base = fp_base p + fp_incr p }
        in runFresh_ (f (runFresh_ x p1)) p2)

instance Functor Fresh where
    fmap f x = Fresh (f . runFresh_ x)

-- | Run a computation in the Fresh monad.
runFresh :: Fresh a -> a
runFresh x = runFresh_ x (FreePool 0 1)

-- | Allocate a fresh id.
fresh :: Fresh FreeID
fresh = Fresh fp_base

-- Return a globally fresh variable.
withfresh :: (FreeID -> a) -> a
withfresh f = unsafePerformIO $ do
    nm <- newFresh
    return (f nm) 

freshSource :: IORef FreeID
freshSource = unsafePerformIO (newIORef 0)
{-# NOINLINE freshSource #-}

newFresh :: IO FreeID
newFresh = do
  r <- atomicModifyIORef freshSource $ \x -> let z = x+1 in (z,z)
  r `seq` return r

