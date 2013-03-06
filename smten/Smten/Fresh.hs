
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Smten.Fresh (
    Fresh(..), runFreshPretty,
    ) where

import Smten.Name
import Smten.Sig

import Control.Monad.State.Strict
import Data.Char(isDigit)
import Data.List(dropWhileEnd)
import qualified Data.Map as Map

class (Functor m, Monad m) => Fresh m where
    -- Return a fresh name based on the given name.
    fresh :: Sig -> m Sig
    

-- Fresh names
--
-- We store a mapping from name to number such that the concatenation of the
-- name and the number is guaranteed to be a fresh name, and the
-- concatenation of the name and any higher number is guaranteed to be a
-- fresh name.

type FreshPretty = State (Map.Map Name Integer)

instance Fresh FreshPretty where
    fresh s@(Sig n t) = do
       let nbase = name $ dropWhileEnd isDigit (unname n)
       m <- get
       let (id, m') = Map.insertLookupWithKey (\_ -> (+)) nbase 1 m
       put $! m'
       case id of
          Nothing -> return $ Sig nbase t
          Just x -> return $ Sig (nbase `nappend` name (show x)) t

runFreshPretty :: FreshPretty a -> a
runFreshPretty x = evalState x Map.empty

