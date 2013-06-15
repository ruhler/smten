
module Smten.AnyMap (
    AnyMap, new, lookup, insert,
    ) where

import Prelude hiding (lookup)

import GHC.Base (Any)
import Data.Functor ((<$>))
import System.Mem.StableName
import Unsafe.Coerce
import qualified Data.HashTable.IO as H

-- AnyMap
--  It maps from keys of any type to values of any type.
--  Different keys and different values may have different types in the same
--  map.
--
--  It is up to the user to guarentee the type of value inserted in the map
--  for a given key is always the type of value looked up for under that given
--  key.
type AnyMap = H.BasicHashTable (StableName Any) Any

new :: IO AnyMap
new = H.new

lookup :: AnyMap -> k -> IO (Maybe v)
lookup m k = do
  knm <- makeStableName $! (unsafeCoerce k)
  fnd <- H.lookup m knm
  return (unsafeCoerce <$> fnd)

insert :: AnyMap -> k -> v -> IO ()
insert m k v = do
  knm <- makeStableName $! (unsafeCoerce k)
  H.insert m knm (unsafeCoerce v)

