
module Smten.Plugin.CG (
   CG, runCG, lift,
   usequalified, getimports,
   gets,
    ) where

import GhcPlugins

import Control.Monad.State

data CGS = CGS {
  -- Accumulated set of imports required for this module.
  cgs_imports :: [String]
}

type CG = StateT CGS CoreM

-- | Use something from the given module in the generated code.
-- This marks the module as requiring an import declaration.
-- Returns the locally qualified name under which that module should be used.
useimport :: String -> CG String
useimport nm = do
  modify $ \s -> s { cgs_imports = nm : cgs_imports s }
  return nm -- for now, return the name unchanged.

-- | Use the given qualified id.
-- usequalified module varname
-- This marks the module as requiring an import declaration, and returns the
-- locally qualified name for the id.
--
-- For example: usequalified "Foo" "bar"
--   Might return: "Foo.bar"
usequalified :: String -> String -> CG String
usequalified m v = do
   m' <- useimport m
   return (m' ++ "." ++ v)

getimports :: CG [String]
getimports = gets cgs_imports

runCG :: CG a -> CoreM a
runCG m = evalStateT m (CGS [])

