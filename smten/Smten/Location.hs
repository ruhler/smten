
module Smten.Location (
    Location(..) 
 ) where

data Location = Location {
    file :: FilePath,
    line :: Integer,
    column :: Integer
} deriving (Eq, Show)

