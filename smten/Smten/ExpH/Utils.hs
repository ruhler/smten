
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.Utils (
    runio,
    ) where

import Data.Maybe

import Smten.Type
import Smten.Name
import Smten.Sig
import Smten.Ppr
import Smten.ExpH.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar

-- | Given a Smten expression of type IO a,
-- returns the Smten expression of type a which results from running the IO
-- computation.
runio :: Thunk -> IO Thunk
runio e
 | Just io <- de_ioEH e = io
 | otherwise = error $ "runio got non-IO: " ++ pretty e

