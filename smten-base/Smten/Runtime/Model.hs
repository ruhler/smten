
{-# LANGUAGE PatternGuards #-}

module Smten.Runtime.Model (
    Model, model, m_vars,
    lookupBoolF,
    ) where

import Smten.Runtime.Formula
import Smten.Runtime.FreeID

data Model = Model {
    m_vars :: [(FreeID, AnyF)]
}

model :: [(FreeID, AnyF)] -> IO Model
model = return . Model

lookupBoolF :: Model -> FreeID -> BoolF
lookupBoolF m nm
  | Just (BoolF x) <- lookup nm (m_vars m) = x
  | otherwise = error "lookupBoolF failed"

