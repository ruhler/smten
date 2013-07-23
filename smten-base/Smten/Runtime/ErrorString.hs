
module Smten.Runtime.ErrorString (
    ErrorString(..), errstr, doerr
 ) where

import Smten.Runtime.Formula

data ErrorString =
   ErrorString String
 | ErrorString_Ite BoolF ErrorString ErrorString

errstr :: String -> ErrorString
errstr = ErrorString

doerr :: ErrorString -> a
doerr (ErrorString msg) = error $ "smten user error: " ++ msg

