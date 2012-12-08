
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Utils (
    runio,
    ) where

import Data.Maybe

import Seri.Type
import Seri.Name
import Seri.Sig
import Seri.Ppr
import Seri.ExpH.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

-- | Given a Seri expression of type IO a,
-- returns the Seri expression of type a which results from running the IO
-- computation.
runio :: ExpH -> IO ExpH
runio e
 | Just (_, msg) <- de_errorEH e = error $ "seri: " ++ msg
 | Just io <- de_ioEH e = io
 | otherwise = error $ "runio got non-IO: " ++ pretty (un_letEH e)

