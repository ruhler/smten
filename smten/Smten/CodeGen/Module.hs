
module Smten.CodeGen.Module (moduleCG) where

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH.Ppr as H
import qualified Language.Haskell.TH.Syntax as H

import Data.Functor ((<$>))

import Smten.Failable
import Smten.Dec
import Smten.Module

import Smten.CodeGen.CG
import Smten.CodeGen.Name
import Smten.CodeGen.Dec
    
moduleCG :: Env -> Module -> Failable H.Doc
moduleCG env mod = do
  let mn = mod_name mod
  body <- decsCG env (mod_decs mod)
  return $
    H.text "{-# LANGUAGE ScopedTypeVariables #-}" H.$+$
    H.text "module" H.<+> H.text (modprefix mn) H.<+> H.text "where" H.$+$
    H.text "import qualified Prelude" H.$+$
    importsCG (mod_imports mod) H.$+$
    H.ppr body

importCG :: Import -> H.Doc
importCG (Import _ fr _ _ _) = H.text $ "import qualified " ++ modprefix fr

importsCG :: [Import] -> H.Doc
importsCG = H.vcat . map importCG

decsCG :: Env -> [Dec] -> Failable [H.Dec]
decsCG env ds = concat <$> runCG env (mapM decCG ds)

