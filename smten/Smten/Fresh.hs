
module Smten.Fresh (Fresh, fresh, runFresh) where

import Smten.Name
import Smten.Sig

import Control.Monad.State.Strict
import Data.Char(isDigit)
import Data.List(dropWhileEnd)
import qualified Data.Map as Map

-- Fresh names
--
-- We store a mapping from name to number such that the concatenation of the
-- name and the number is guaranteed to be a fresh name, and the
-- concatenation of the name and any higher number is guaranteed to be a
-- fresh name.

type Fresh = State (Map.Map Name Integer)

fresh :: Sig -> Fresh Sig
fresh s@(Sig n t) = do
   let nbase = name $ dropWhileEnd isDigit (unname n)
   m <- get
   let (id, m') = Map.insertLookupWithKey (\_ -> (+)) nbase 1 m
   put $! m'
   case id of
      Nothing -> return $ Sig nbase t
      Just x -> return $ Sig (nbase `nappend` name (show x)) t

runFresh :: Fresh a -> a
runFresh x = evalState x Map.empty

