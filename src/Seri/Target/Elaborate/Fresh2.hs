-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

module Seri.Target.Elaborate.Fresh2 (
    Fresh, runFresh, fresh,
    ) where

import Control.Monad.State.Strict

import Data.Char(isDigit)
import Data.List(dropWhileEnd)
import qualified Data.Map as Map

import Seri.Lambda

-- Fresh names
--
-- We store a mapping from name to number such that the concatenation of the
-- name and the number is guaranteed to be a fresh name, and the
-- concatenation of the name and any higher number is guaranteed to be a
-- fresh name.

type Fresh = State Integer

-- return a fresh name based on the given name.
fresh :: Sig -> Fresh Sig
fresh s@(Sig n t) = do
   id <- get
   put $! id + 1
   return (Sig (n ++ show id) t)

runFresh :: Fresh a -> [Name] -> a
runFresh x nms = evalState x (freshmap nms)

-- construct the initial fresh map for use with fresh variables.
freshmap :: [Name] -> Integer
freshmap ns =
  let idof :: Name -> Integer
      idof n = 
        let digits = takeWhile isDigit (reverse n)
        in if null digits then 0 else read (reverse digits)
  in 1 + foldr (\a b -> max (idof a) b) 0 ns


