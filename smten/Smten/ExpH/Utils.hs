
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.Utils (
    runio,
    ) where

import Smten.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar
import Smten.ExpH.Ppr ()

-- | Given a Smten expression of type IO a,
-- returns the Smten expression of type a which results from running the IO
-- computation.
runio :: ExpH -> IO ExpH
runio e
 | Just io <- de_ioEH e = io
 | ErrorEH s <- force e = error s
 | otherwise = error $ "runio: " ++ pretty e

