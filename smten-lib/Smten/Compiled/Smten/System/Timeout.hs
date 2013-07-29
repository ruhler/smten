
module Smten.Compiled.Smten.System.Timeout (timeout) where

import qualified Prelude as P
import qualified System.Timeout as P
import Smten.Compiled.Smten.Smten.Base
import Smten.Compiled.Smten.Data.Maybe
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

timeout :: (SmtenHS0 a) => Int -> P.IO a -> P.IO (Maybe a)
timeout = symapp (\t x -> do
    v <- P.timeout t x
    case v of
        P.Just x -> P.return (Just x)
        P.Nothing -> P.return Nothing
    )

