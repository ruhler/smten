
module Smten.Plugin.CG (
   CG, runCG, lift,
   addimport, getimports,
   gets,
    ) where

import GhcPlugins

import Control.Monad.State

data CGS = CGS {
  -- Accumulated set of imports required for this module.
  cgs_imports :: [String]
}

type CG = StateT CGS CoreM

addimport :: String -> CG ()
addimport nm = modify $ \s -> s { cgs_imports = nm : cgs_imports s }

getimports :: CG [String]
getimports = gets cgs_imports

runCG :: CG a -> CoreM a
runCG m = evalStateT m (CGS [])

