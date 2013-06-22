
module Smten.Tests.Test (test) where

test :: String -> Bool -> IO ()
test _ True = return ()
test msg False = error msg

