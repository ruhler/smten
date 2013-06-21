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

module Smten.Loader (SearchPath, load) where

import System.Directory

import Control.Monad.State
import qualified Data.HashSet as Set
import Data.Functor((<$>))

import Smten.Name
import Smten.Location
import Smten.Dec
import Smten.Module
import Smten.Parser
import Smten.Failable

type SearchPath = [FilePath]

data LS = LS {
    -- The set of modules already loaded
    ls_loaded :: Set.Set Name,

    -- The list of loaded modules stored in dependency order.
    -- Modules in the tail of the list do not depend on the head of the list.
    ls_mods :: [Module],

    -- The module search path
    ls_search :: SearchPath
}

type LSM = StateT LS IO

-- | Load the given module and all modules required for the given module.
loads :: Location -> Name -> LSM ()
loads l mn = do
  loaded <- gets ls_loaded
  if mn `Set.member` loaded
    then return ()
    else do
      sp <- gets ls_search
      m <- lift $ loadone sp l mn
      loadneeded m

-- | Load all modules needed for the given module, and add the given module to
-- the list of loaded modules.
loadneeded :: Module -> LSM ()
loadneeded m = do
  modify $ \s -> s { ls_loaded = Set.insert (mod_name m) (ls_loaded s) }
  sequence_ [loads (imp_loc i) (imp_from i) | i <- mod_imports m]
  modify $ \s -> s { ls_mods = m : ls_mods s }

-- | Load a single module with the given name.
loadone :: SearchPath -> Location -> Name -> IO Module
loadone sp l n = do
    fname <- findmodule sp l n
    loadmod fname

-- | Load a single module from the given file.
loadmod :: FilePath -> IO Module
loadmod fname = do
    text <- readFile fname
    m <- attemptIO $ parse fname text
    return $ if (mod_name m == name "Prelude")
                then m { mod_decs = prelude ++ mod_decs m }
                else m { mod_imports = Import lunknown (name "Prelude") (name "Prelude") False (Exclude []): mod_imports m }

findmodule :: SearchPath -> Location -> Name -> IO FilePath
findmodule [] l n = attemptIO $ throw (lmsg l ("Module " ++ unname n ++ " not found"))
findmodule (s:ss) l n =
 let dirify :: String -> FilePath
     dirify n =
        case n of
          [] -> []
          ('.':xs) -> '/' : dirify xs
          (x:xs) -> x : dirify xs

     fp = s ++ "/" ++ dirify (unname n) ++ ".smtn"
 in do
    exists <- doesFileExist fp
    if exists
        then return fp
        else findmodule ss l n

-- | Load the complete module hierarchy needed for the smtn file specified in
-- the given path.
--
-- Returns the modules in dependency order: The head module does not depend on
-- any of the tail modules.
load :: SearchPath -> FilePath -> IO [Module]
load path mainmod = {-# SCC "Load" #-} do
    main <- loadmod mainmod
    reverse . ls_mods <$> execStateT (loadneeded main) (LS Set.empty [] path)

