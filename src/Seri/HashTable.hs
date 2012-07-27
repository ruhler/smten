-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

-- | A hash table which is used only for lookups.
module Seri.HashTable (
    HashTable(), table, Seri.HashTable.lookup,
    ) where

import Data.Array
import Data.Hashable

data HashTable k v = HashTable {
    size :: Int,
    elems :: Array Int [(k, v)]
}


-- A list of the hash table sizes we'll draw from.
-- From http://planetmath.org/GoodHashTablePrimes.html 
sizes :: [Int]
sizes = [
    193, 389, 769, 1543, 3079, 6151, 12289, 24593, 49157, 98317, 196613,
    196613, 393241, 786433, 1572869, 3145739, 6291469, 12582917, 25165843,
    50331653, 100663319]

-- | Construct a new table with the given key/value pair.
table :: (Hashable k) => [(k, v)] -> HashTable k v
table elems =
  let s = head (filter (> (2 * length elems)) sizes ++ [last sizes])
      assocs = [(indexof s k, (k,v)) | (k, v) <- elems]
  in HashTable s (accumArray (\e a -> a:e) [] (0, s) assocs)

-- | Lookup the value for the given key in the table.
lookup :: (Eq k, Hashable k) => k -> HashTable k v -> Maybe v
lookup k (HashTable s es) = Prelude.lookup k (es ! indexof s k)

-- | Return the index of the bucket where the key should be found given the
-- size of the hash table.
indexof :: (Hashable k) => Int -> k -> Int
indexof s k = hash k `mod` s


