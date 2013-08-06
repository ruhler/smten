
module Smten.Compiled.Smten.System.Environment (getArgs) where

import qualified Prelude as P
import qualified System.Environment as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Runtime.SymbolicOf

getArgs :: P.IO (List__ (List__ Char))
getArgs = do
    args <- P.getArgs
    P.return (tosym (P.map fromHSString args))

