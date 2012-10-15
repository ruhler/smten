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

module Seri.Lambda.Loader (SearchPath, load) where

import System.Directory

import Data.List(nub)

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Parser
import Seri.Lambda.Modularity
import Seri.Lambda.Sugar
import Seri.Lambda.Ppr

type SearchPath = [FilePath]

-- | Load a bunch of modules.
-- Loads as many modules as are needed based on the import list and imports in
-- any modules which are loaded.
loads :: SearchPath
      -> [Name]     -- ^ List of module names to load
      -> [Module]   -- ^ List of modules already loaded
      -> IO [Module]
loads _ [] ms = return ms
loads sp ns ms =
  let isLoaded :: Name -> Bool
      isLoaded n = n `elem` ([mn | Module mn _ _ <- ms])

      needed = nub $ filter (not . isLoaded) ns
  in do
    loaded <- mapM (loadone sp) needed
    let newimports = concat [i | Module _ i _ <- loaded]
    let newnames = [n | Import n <- newimports]
    loads sp newnames (loaded ++ ms)

-- | Load a single module with the given name.
loadone :: SearchPath -> Name -> IO Module
loadone sp n = do
    fname <- findmodule sp n
    text <- readFile fname
    attemptIO $ parse fname text
      
findmodule :: SearchPath -> Name -> IO FilePath
findmodule [] n = fail $ "Module " ++ pretty n ++ " not found"
findmodule (s:ss) n =
 let dirify :: Name -> FilePath
     dirify n | nnull n = []
     dirify n | nhead n == '.' = '/' : dirify (ntail n)
     dirify n = nhead n : dirify (ntail n)

     fp = s ++ "/" ++ dirify n ++ ".sri"
 in do
    exists <- doesFileExist fp
    if exists
        then return fp
        else do
            putStrLn $ fp ++ " does not exist"
            findmodule ss n

-- | Load the complete module hierarchy needed for the sri file specified in
-- the given path.
load :: SearchPath -> FilePath -> IO [Module]
load path mainmod = do
    maintext <- readFile mainmod
    main@(Module _ imps _)  <- attemptIO $ parse mainmod maintext
    loads path [n | Import n <- imps] [main]

