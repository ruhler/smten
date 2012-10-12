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

module Seri.Elaborate.FreshFast (
    Fresh, runFresh, fresh,
    ) where

import Control.Monad.State.Strict
import Data.ByteString.Char8 as S

import Data.Char(isDigit)
import Data.List(dropWhileEnd)
import qualified Data.Map as Map

import Seri.Lambda

type Fresh = State Name

-- return a fresh name based on the given name.
fresh :: Sig -> Fresh Sig
fresh s@(Sig _ t) = do
   id <- get
   put $! incrnm id
   return (Sig id t)

runFresh :: Fresh a -> [Name] -> a
runFresh x nms = evalState x (name "~Ea")

-- TODO: this assumes name is a Char8 bytestring.
-- Is that a bad idea?
-- 
-- Name is of the form "~E<num>"
-- We want to increment the number.
--
-- Only, instead of numbers made of digits, we use numbers made of lowercase
-- letters (which gives us more options before extending the string size)
incrnm :: Name -> Name
incrnm n = 
  let start = S.init n
      end = S.last n
  in case end of
        'z' -> S.snoc (incrnm start) 'a'
        'E' -> name "~Ea"
        _ -> S.snoc start (succ end)
        

