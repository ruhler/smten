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

{-# LANGUAGE PatternGuards #-}

-- Back end target which translates smten programs into Haskell. Supports
-- symbolic computation.
module Smten.HaskellF.Compile (
    haskellf, hfData,
    ) where

import Data.Hash.MD5

import Data.Functor((<$>))
import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H
import System.Directory

import Smten.Failable
import Smten.Name
import Smten.Dec
import Smten.Module
import Smten.Ppr
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Data
import Smten.HaskellF.Compile.Dec
import Smten.HaskellF.Compile.Ppr
import Smten.HaskellF.Compile.Module

-- haskell odir env mods
--  Compile the given declarations to haskell.
haskellf :: FilePath -> [Module] -> IO ()
haskellf odir mods = {-# SCC "HaskellF" #-} do
  let env = environ mods
      mkmod m = do
        hf <- pretty <$> attemptIO (hsModule env m)
        let dst = map dot2slash . unname  . hfpre . mod_name $ m
            tgt = odir ++ "/" ++ dst ++ ".hs"
        createDirectoryIfMissing True (directory tgt)
        writeFileIfChanged tgt hf
  mapM_ mkmod mods

-- Write to the given file, but only if we are writing something new.
-- This keeps us from touching the timestamp of the file if there is no
-- change, so ghc won't have to recompile that file.
writeFileIfChanged :: FilePath -> String -> IO ()
writeFileIfChanged tgt cnts = do
   exists <- doesFileExist tgt
   if exists 
        then do
            old <- readFile tgt
            if md5 (Data.Hash.MD5.Str old) == md5 (Data.Hash.MD5.Str cnts)
                then return ()
                else writeFile tgt cnts
        else writeFile tgt cnts

directory :: FilePath -> FilePath
directory f
  | '/' `notElem` f = ""
  | (h, t) <- break (== '/') f = h ++ "/" ++ directory (tail t)

dot2slash :: Char -> Char
dot2slash '.' = '/'
dot2slash c = c 

hfData :: Name -> [TyVar] -> [Con] -> Failable [H.Dec]
hfData n tyvars constrs = runHF (mkEnv []) (hsData n tyvars constrs)

