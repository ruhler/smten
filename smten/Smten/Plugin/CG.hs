
module Smten.Plugin.CG (
   CG, runCG, lift,
   usequalified, getimports,
   gets,
    ) where

import GhcPlugins

import Control.Monad.State
import qualified Data.Map as Map

data CGS = CGS {
  -- The name of the module for which code generation is being done.
  -- This is used to avoid giving a qualified name to local entities.
  cgs_modnm :: String,

  -- Accumulated set of imports required for this module.
  -- Maps the fully qualified name to the locally qualified name.
  -- For locally qualified names we use M0, M1, ...
  cgs_imports :: Map.Map String String
}

type CG = StateT CGS CoreM


-- | Use the given qualified id.
-- usequalified module varname
-- This marks the module as requiring an import declaration, and returns the
-- locally qualified name for the id.
--
-- For example: usequalified "Foo" "bar"
--   Might return: "Foo.bar"
usequalified :: String -> String -> CG String
usequalified nm v = do
  this <- gets cgs_modnm
  if nm == this
     then return v
     else do
       m <- gets cgs_imports
       case Map.lookup nm m of
          Just qlf -> return (qlf ++ "." ++ v)
          Nothing -> do
            let qlf = "M" ++ show (Map.size m)
            modify $ \s -> s { cgs_imports = Map.insert nm qlf m }
            return (qlf ++ "." ++ v)

-- | Get the import declarations required for this module.
-- Returns imports as a list of tuples where the first element of the tuple is
-- the fully qualified name of the module to import, and the second element of
-- the tuple is the name under which to import the module as.
getimports :: CG [(String, String)]
getimports = gets (Map.toList . cgs_imports)

-- | Run code generation.
-- runCG m x
--   m - the name of the module being generated.
--   x - the code generation to run.
runCG :: String -> CG a -> CoreM a
runCG nm m = evalStateT m (CGS ("Smten.Compiled." ++ nm) Map.empty)

