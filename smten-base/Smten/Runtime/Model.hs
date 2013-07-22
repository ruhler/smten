
{-# LANGUAGE PatternGuards #-}

module Smten.Runtime.Model (
    Model, model, m_cached,
    lookupBoolF,
    ) where

import System.IO.Unsafe
import qualified Smten.Runtime.AnyMap as A

import Smten.Runtime.Formula
import Smten.Runtime.FreeID

data Model = Model {
    m_vars :: [(FreeID, AnyF)],
    m_cache :: A.AnyMap
}

model :: [(FreeID, AnyF)] -> IO Model
model vars = do
   cache <- A.new
   return (Model vars cache)

-- lookup the value of an object under the given model.
-- The lookup is memoized.
m_cached :: Model -> (Model -> a -> b) -> a -> b
m_cached m f x = unsafeDupablePerformIO $ do
    let mc = m_cache m
    xfnd <- A.lookup mc x
    case xfnd of
       Just v -> return v
       Nothing -> do
         let v = f m x
         A.insert mc x v
         return v

lookupBoolF :: Model -> FreeID -> BoolF
lookupBoolF m nm
  | Just (BoolF x) <- lookup nm (m_vars m) = x
  | otherwise = error "lookupBoolF failed"

