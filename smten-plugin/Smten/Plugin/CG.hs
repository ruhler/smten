
module Smten.Plugin.CG (
   CG, runCG, lift,
   addimport, getimports,
   withtype, withtypes,
   cgs_types, gets,
   subst,
    ) where

import Data.Functor
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
  t' <- subst t
  ts <- gets cgs_types
  modify $ \s -> s { cgs_types = (tyv, t') : ts }
  v <- q
  modify $ \s -> s { cgs_types = ts }
  return v
  
withtypes :: [(TyVar, Type)] -> CG a -> CG a
withtypes [] q = q
withtypes ((tyv, t):xs) q = withtype tyv t (withtypes xs q)

subst :: Type -> CG Type
subst t = do
    (tyvs, ts) <- unzip <$> gets cgs_types
    return $ substTyWith tyvs ts t

