
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
  H.text ("module " ++ unname modname
            ++ "(module " ++ unname modname ++ ") where") H.$+$
  H.text "import qualified Prelude" H.$+$
  H.text "import qualified Smten.HaskellF.HaskellF as S" H.$+$
  H.text "import qualified Smten.Name as S" H.$+$
  H.text "import qualified Smten.Type as S" H.$+$
  H.text "import qualified Smten.ExpH as S" H.$+$
  H.text "import qualified Smten.HaskellF.Lib.Numeric as S" H.$+$
  H.text "import qualified Smten.HaskellF.Lib.Prelude" H.$+$
  primimports modname

primimports :: Name -> H.Doc
primimports n
  | n == name "Smten.Lib.Prelude"
      = H.text "import qualified Smten.HaskellF.Lib.Prelude" H.$+$
        H.text "import Smten.HaskellF.Lib.Prelude as Smten.Lib.Prelude"
  | n == name "Smten.Lib.Debug.Trace"
      = H.text "import Smten.HaskellF.Lib.Trace as Smten.Lib.Debug.Trace"
  | n == name "Smten.Lib.Smten.Bit"
      = H.text "import Smten.HaskellF.Lib.Bit as Smten.Lib.Smten.Bit"
  | n == name "Smten.Lib.Smten.SMT.Symbolic"
      = H.text "import Smten.HaskellF.Lib.Symbolic as Smten.Lib.Smten.SMT.Symbolic"
  | otherwise = H.empty

hsImport :: Import -> H.Doc
hsImport (Import fr _ _ _) = H.text $ "import qualified " ++ unname (hfpre fr)

hsImports :: [Import] -> H.Doc
hsImports = H.vcat . map hsImport

hsDecls :: Env -> [Dec] -> Failable [H.Dec]
hsDecls env ds = runHF env (concat <$> mapM hsDec ds)

hsModule :: Env -> Module -> Failable H.Doc
hsModule env mod = do
  let header = hsHeader (hfpre $ mod_name mod)
      mn = mod_name mod
      main = case attemptM $ lookupValD env (qualified mn (name "main")) of
               Just _ -> H.text "main__ = Smten.HaskellF.Lib.Prelude.__main_wrapper main"
               Nothing -> H.empty
      imports = hsImports (mod_imports mod)
  hdecls <- hsDecls env (mod_decs mod)
  return (header H.$+$ imports H.$+$ H.ppr hdecls H.$+$ main)

