
{-# LANGUAGE PatternGuards #-}

module Smten.HaskellF.Compile.Module (
    hsModule
    ) where

import Data.Functor((<$>))
import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH.Ppr as H
import qualified Language.Haskell.TH.Syntax as H

import Smten.Failable
import Smten.Name
import Smten.Dec
import Smten.Module
import Smten.HaskellF.Compile.HF
import Smten.HaskellF.Compile.Name
import Smten.HaskellF.Compile.Dec

-- Produce common header for all modules.
-- Includes language pragmas and HaskellF imports.
hsHeader :: Name -> H.Doc
hsHeader modname = 
  H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
  H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
  H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
  H.text "{-# LANGUAGE FlexibleContexts #-}" H.$+$
  H.text "{-# LANGUAGE UndecidableInstances #-}" H.$+$
  H.text "{-# LANGUAGE ScopedTypeVariables #-}" H.$+$
  H.text "{-# LANGUAGE InstanceSigs #-}" H.$+$
  H.text ("module " ++ unname modname ++ " where") H.$+$
  H.text "import qualified Prelude" H.$+$
  H.text "import qualified Smten.HaskellF.HaskellF as S" H.$+$
  H.text "import qualified Smten.Name as S" H.$+$
  H.text "import qualified Smten.Type as S" H.$+$
  H.text "import qualified Smten.ExpH as S" H.$+$
  H.text "import Smten.HaskellF.Lib.Prelude" H.$+$
  H.text "import Smten.HaskellF.Lib.Symbolic"

hsImport :: Import -> H.Doc
hsImport (Import fr _) = H.text $ "import " ++ unname (hfpre fr)

hsImports :: [Import] -> H.Doc
hsImports = H.vcat . map hsImport

-- Given an untyped declaration from the module, get its typed version from
-- the environment.
typedModDecl :: Env -> Name -> Dec -> Failable Dec
typedModDecl env mn d
  | ValD _ (TopExp (TopSig n _ _) _) <- d = lookupValD env (qualified mn n)
  | DataD _ n _ _ <- d = lookupDataD env n
  | ClassD _ _ n _ _ <- d = lookupClassD env n
  | InstD _ _ cls _ <- d = lookupInstD env cls
  | PrimD _ (TopSig n _ _) <- d = lookupPrimD env (qualified mn n)

hsDecls :: Env -> [Dec] -> Failable [H.Dec]
hsDecls env ds = runHF env (concat <$> mapM hsDec ds)

hsModule :: Env -> Module -> Failable H.Doc
hsModule env mod = do
  let header = hsHeader (hfpre $ mod_name mod)
      mn = mod_name mod
      main = case attemptM $ lookupValD env (qualified mn (name "main")) of
               Just _ -> H.text "main__ = __main_wrapper main"
               Nothing -> H.empty
      imports = hsImports (mod_imports mod)
  tdecls <- mapM (typedModDecl env mn) (mod_decs mod)
  hdecls <- hsDecls env tdecls
  return (header H.$+$ imports H.$+$ H.ppr hdecls H.$+$ main)

