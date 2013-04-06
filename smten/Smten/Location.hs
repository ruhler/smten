
module Smten.Location (
    Location(..), Locate(..), lunknown, lmsg,
 ) where

data Location = Location {
    file :: FilePath,
    line :: Integer,
    column :: Integer
} deriving (Eq, Show)

class Locate a where
    locate :: a -> Location

lmsg :: Location -> String -> String
lmsg loc msg = 
  let fp = file loc
      ln = line loc
      cl = column loc
  in fp ++ ":" ++ show ln ++ ":" ++ show cl ++ ": " ++ msg

lunknown :: Location
lunknown = Location "unknown" 0 0

