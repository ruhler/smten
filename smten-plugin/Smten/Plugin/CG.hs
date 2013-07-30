
module Smten.Plugin.CG (
   CG, runCG, lift,
   addimport, getimports,
   withlocal, withlocals, islocal,
   withtype,
   cgs_types, gets,
    ) where

import GhcPlugins

import Control.Monad.State
import Data.Functor

data CGS = CGS {
  -- Accumulated set of imports required for this module.
  cgs_imports :: [String],

  -- local variables in scope.
  cgs_locals :: NameSet,

  -- type substitutions to perform.
  cgs_types :: [(TyVar, Type)]
}

type CG = StateT CGS CoreM

addimport :: String -> CG ()
addimport nm = modify $ \s -> s { cgs_imports = nm : cgs_imports s }

getimports :: CG [String]
getimports = gets cgs_imports

withlocals :: [Name] -> CG a -> CG a
withlocals nms q = do
  m <- gets cgs_locals
  modify $ \s -> s { cgs_locals = addListToNameSet m nms }
  v <- q
  modify $ \s -> s { cgs_locals = m }
  return v

withlocal :: Name -> CG a -> CG a
withlocal nm = withlocals [nm]

islocal :: Name -> CG Bool
islocal nm = elemNameSet nm <$> gets cgs_locals

runCG :: CG a -> CoreM a
runCG m = evalStateT m (CGS [] emptyNameSet [])

withtype :: TyVar -> Type -> CG a -> CG a
withtype tyv t q = do
  ts <- gets cgs_types
  modify $ \s -> s { cgs_types = (tyv, t) : ts }
  v <- q
  modify $ \s -> s { cgs_types = ts }
  return v
  

