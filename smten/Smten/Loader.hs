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

module Smten.Loader (SearchPath, loadmod, loadmods, loadenv) where

import System.Directory

import Data.Functor((<$>))
import Data.List(nub)

import Smten.Name
import Smten.Dec
import Smten.Module
import Smten.Parser
import Smten.Failable
import Smten.Typing

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
      isLoaded n = n `elem` map mod_name ms

      needed = nub $ filter (not . isLoaded) ns
  in do
    loaded <- mapM (loadone sp) needed
    let newimports = concatMap mod_imports loaded
    let newnames = [n | Import n <- newimports]
    loads sp newnames (loaded ++ ms)

-- | Load a single module with the given name.
loadone :: SearchPath -> Name -> IO Module
loadone sp n = do
    fname <- findmodule sp n
    loadmod fname

-- | Load a single module from the given file.
loadmod :: FilePath -> IO Module
loadmod fname = do
    text <- readFile fname
    attemptIO $ addprelude <$> parse fname text

-- Add the prelude import to a module if needed.
addprelude :: Module -> Module
addprelude m
  | mod_name m == name "Prelude" = m
  | otherwise = m { mod_imports = Import (name "Prelude") : mod_imports m }
      
findmodule :: SearchPath -> Name -> IO FilePath
findmodule [] n = fail $ "Module " ++ unname n ++ " not found"
findmodule (s:ss) n =
 let dirify :: Name -> FilePath
     dirify n | nnull n = []
     dirify n | nhead n == '.' = '/' : dirify (ntail n)
     dirify n = nhead n : dirify (ntail n)

     fp = s ++ "/" ++ dirify n ++ ".smtn"
 in do
    exists <- doesFileExist fp
    if exists
        then return fp
        else findmodule ss n

-- | Load the complete module hierarchy needed for the smtn file specified in
-- the given path.
loadmods :: SearchPath -> FilePath -> IO [Module]
loadmods path mainmod = do
    main <- loadmod mainmod
    loads path [n | Import n <- mod_imports main] [main]


-- Load a program into an environment.
-- Performs module flattening, type inference, and type checking.
loadenv :: SearchPath -> FilePath -> IO Env
loadenv path fin = do
    mods <- loadmods path fin
    flat <- attemptIO $ {-# SCC "Flatten" #-} flatten mods
    kinded <- attemptIO $ {-# SCC "KindInfer" #-} kindinfer (mkEnv flat)
    decs <- attemptIO $ {-# SCC "TypeInfer" #-} typeinfer (mkEnv kinded) kinded
    let env = mkEnv decs
    attemptIO $ {-# SCC "TypeCheck" #-} typecheck env decs
    return env

