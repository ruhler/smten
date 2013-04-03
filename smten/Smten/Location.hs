
{-# LANGUAGE FlexibleContexts #-}

module Smten.Location (
    Location(..), Locate(..), lthrow, lunknown, lmsg,
 ) where

import Control.Monad.Error

import Smten.Failable

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

-- | Fail with a message augmented with location information.
lthrow :: (MonadError String m) => Location -> String -> m a
lthrow loc msg = throw $ lmsg loc msg

lunknown :: Location
lunknown = Location "unknown" 0 0

