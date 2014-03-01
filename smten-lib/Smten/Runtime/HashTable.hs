
-- | A hash table which is used only for lookups.
module Smten.Runtime.HashTable (
    HashTable(), table,
    Smten.Runtime.HashTable.lookup,
    Smten.Runtime.HashTable.assocs,
    ) where

import Data.Array
import Data.Hashable

data HashTable k v = HashTable {
    _size :: Int,
    _elems :: Array Int [(k, v)]
}


-- A list of the hash table sizes we'll draw from.
-- From http://planetmath.org/GoodHashTablePrimes.html 
sizes :: [Int]
sizes = [
    193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613,
    196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843,
    50331653, 100663319]

-- | Construct a new table with the given key/value pair.
-- The first value for each key in the list is stored.
table :: (Hashable k) => [(k, v)] -> HashTable k v
table elems =
  let s = head (filter (> (2 * length elems)) sizes ++ [last sizes])
      assocs = [(indexof s k, (k,v)) | (k, v) <- elems]
  in HashTable s (accumArray (\e a -> e ++ [a]) [] (0, s) assocs)

-- | Lookup the value for the given key in the table.
lookup :: (Eq k, Hashable k) => k -> HashTable k v -> Maybe v
lookup k (HashTable s es) = Prelude.lookup k (es ! indexof s k)

-- | Return the index of the bucket where the key should be found given the
-- size of the hash table.
indexof :: (Hashable k) => Int -> k -> Int
indexof s k = hash k `mod` s

assocs :: HashTable k v -> [(k, v)]
assocs (HashTable _ es) = concat (Data.Array.elems es)

