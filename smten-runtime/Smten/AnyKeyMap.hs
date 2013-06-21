
module Smten.AnyKeyMap (
    AnyKeyMap, new, lookup, insert,
    ) where

import Prelude hiding (lookup)

import qualified Smten.AnyMap as A

type AnyKeyMap v = A.AnyMap

new :: IO (AnyKeyMap v)
new = A.new

lookup :: AnyKeyMap v -> k -> IO (Maybe v)
lookup = A.lookup

insert :: AnyKeyMap v -> k -> v -> IO ()
insert = A.insert

