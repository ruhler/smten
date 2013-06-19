
{-# LANGUAGE MultiParamTypeClasses #-}

module Smten.Runtime.IO (IO, runio) where

import qualified Prelude as P
import Prelude hiding (IO)

import Data.Functor((<$>))

import Smten.Runtime.SmtenHS as S

data IO a = IO (P.IO a)
          | IO_Prim (Assignment -> IO a) (IO a)
          | IO_Ite S.Bool (IO a) (IO a)
          | IO_Error ErrorString

instance (Haskelly ha sa) => Haskelly (P.IO ha) (IO sa) where
    frhs x = IO (frhs <$> x)

    tohs (IO x) = tohs <$> x
    tohs _ = error "tohs.IO failed"

instance SmtenHS1 IO where
    realize1 m (IO x) = IO (realize m <$> x)
    realize1 m (IO_Prim r _) = realize m (r m)
    realize1 m (IO_Ite p a b) = __caseTrue (realize m p) (realize m a) (realize m b)
    realize1 m x@(IO_Error msg) = x

    sapp1 f x =
      case x of
        IO {} -> f x
        IO_Ite p a b -> ite p (sapp f a) (sapp f b)
        IO_Error msg -> error0 msg
        IO_Prim r c -> primsapp f r c
    primitive1 = IO_Prim
    error1 = IO_Error
    ite1 = IO_Ite

runio :: IO a -> P.IO ()
runio (IO x) = x >> return ()
runio (IO_Error msg) = doerr msg

