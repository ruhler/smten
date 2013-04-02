
{-# LANGUAGE FlexibleContexts #-}

module Smten.Location (
    Location(..), Locate(..), lthrow,
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

-- | Fail with a message augmented with location information.
lthrow :: (MonadError String m) => Location -> String -> m a
lthrow loc msg =
  let fp = file loc
      ln = line loc
      cl = column loc
  in throw $ fp ++ ":" ++ show ln ++ ":" ++ show cl ++ ": " ++ msg

