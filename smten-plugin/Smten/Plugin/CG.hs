
module Smten.Plugin.CG (
   CG, runCG, lift,
   addimport, getimports,
   addexport, getexports,
   gets,
    ) where

import GhcPlugins

import Control.Monad.State
import qualified Smten.Plugin.Output.Syntax as S

data CGS = CGS {
  -- Accumulated set of imports required for this module.
  cgs_imports :: [String],

  -- Accumulated set of exports required for this module.
  cgs_exports :: [S.Export]
}

type CG = StateT CGS CoreM

addimport :: String -> CG ()
addimport nm = modify $ \s -> s { cgs_imports = nm : cgs_imports s }

getimports :: CG [String]
getimports = gets cgs_imports

addexport :: S.Export -> CG ()
addexport x = modify $ \s -> s { cgs_exports = x : cgs_exports s }

getexports :: CG [S.Export]
getexports = gets cgs_exports

runCG :: CG a -> CoreM a
runCG m = evalStateT m (CGS [] [])

