
module Seri.Lambda.Loader (load) where

import System.Directory

import Data.List(nub)

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Parser
import Seri.Lambda.Sugar

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
findmodule [] n = fail $ "Module " ++ n ++ " not found"
findmodule (s:ss) n =
 let dirify :: Name -> FilePath
     dirify [] = []
     dirify ('.':r) = '/' : dirify r
     dirify (c:r) = c : dirify r

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

