
module Smten.Plugin.CG (
   CG, runCG, lift,
   addimport, getimports,
   withtype,
   cgs_types, gets,
    ) where

import GhcPlugins

import Control.Monad.State

data CGS = CGS {
  -- Accumulated set of imports required for this module.
  cgs_imports :: [String],

  -- type substitutions to perform.
  cgs_types :: [(TyVar, Type)]
}

type CG = StateT CGS CoreM

addimport :: String -> CG ()
addimport nm = modify $ \s -> s { cgs_imports = nm : cgs_imports s }

getimports :: CG [String]
getimports = gets cgs_imports

runCG :: CG a -> CoreM a
runCG m = evalStateT m (CGS [] [])

withtype :: TyVar -> Type -> CG a -> CG a
withtype tyv t q = do
  ts <- gets cgs_types
  modify $ \s -> s { cgs_types = (tyv, t) : ts }
  v <- q
  modify $ \s -> s { cgs_types = ts }
  return v
  

