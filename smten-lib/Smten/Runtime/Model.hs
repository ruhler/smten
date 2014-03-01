
module Smten.Runtime.Model (
    Any(..),
    Model, model, lookupBool, lookupInteger, lookupBit,
    m_bools, m_integers, m_bits,
  ) where

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import qualified Smten.Runtime.HashTable as HT

data Any = BoolA Bool | IntegerA Integer | BitA Bit

newtype Model = Model {
    m_hashed :: HT.HashTable FreeID Any
}

m_vars :: Model -> [(FreeID, Any)]
m_vars m = HT.assocs (m_hashed m)

m_bools :: Model -> [(FreeID, Bool)]
m_bools m = [(nm, v) | (nm, BoolA v) <- m_vars m]

m_bits :: Model -> [(FreeID, Bit)]
m_bits m = [(nm, v) | (nm, BitA v) <- m_vars m]

m_integers :: Model -> [(FreeID, Integer)]
m_integers m = [(nm, v) | (nm, IntegerA v) <- m_vars m]

instance Show Model where
    show = show . map fst . m_vars

model :: [(FreeID, Any)] -> IO Model
model vars = return (Model (HT.table vars))

-- | Look up the boolean value for the given free variable in the model.
-- The given variable is assumed to have boolean type.
lookupBool :: Model -> FreeID -> Bool
lookupBool m nm =
  case HT.lookup nm (m_hashed m) of
    Just (BoolA x) -> x
    Just _ -> error "lookupBool: type mismatch"
    Nothing -> False    -- any value will do for the default.

-- | Look up the integer value for the given free variable in the model.
-- The given variable is assumed to have integer type.
lookupInteger :: Model -> FreeID -> Integer
lookupInteger m nm =
  case HT.lookup nm (m_hashed m) of
    Just (IntegerA x) -> x
    Just _ -> error "lookupInteger: type mismatch"
    Nothing -> 0        -- any value will do for the default.

-- | Look up the bit vector value for the given free variable in the model.
-- The given variable is assumed to have bit type for the given width.
lookupBit :: Model -> Integer -> FreeID -> Bit
lookupBit m w nm =
  case HT.lookup nm (m_hashed m) of
    Just (BitA x) -> x
    Just _ -> error "lookupBit: type mismatch"
    Nothing -> bv_make w 0 -- any value will do for the default

