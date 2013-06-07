
module Smten.CodeGen.Module (moduleCG) where

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH.Ppr as H
import qualified Language.Haskell.TH.Syntax as H

import Data.Functor ((<$>))
import Data.List(nub)
import Data.Maybe(catMaybes)

import Smten.Failable
import Smten.Dec
import Smten.Name
import Smten.Module

import Smten.CodeGen.Annotates
import Smten.CodeGen.CG
import Smten.CodeGen.Name
import Smten.CodeGen.Dec
    
moduleCG :: Env -> Module -> Failable H.Doc
moduleCG env mod = do
  let mn = mod_name mod
  body <- decsCG env (mod_decs mod)
  return $
    H.text "{-# LANGUAGE DataKinds #-}" H.$+$
    H.text "{-# LANGUAGE FlexibleInstances #-}" H.$+$
    H.text "{-# LANGUAGE InstanceSigs #-}" H.$+$
    H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
    H.text "{-# LANGUAGE ScopedTypeVariables #-}" H.$+$
    H.text "{-# LANGUAGE TypeSynonymInstances #-}" H.$+$
    H.text "module" H.<+> H.text (modprefix mn) H.<+>
        H.parens ( H.text "module" H.<+> H.text (modprefix mn)
        ) H.<+> H.text "where" H.$+$
    H.text "import qualified Prelude" H.$+$
    H.text "import qualified Smten.Runtime.Builtin as Smten" H.$+$
    importsCG (mod_imports mod) H.$+$
    primportsCG (modprefix mn) (mod_decs mod) H.$+$
    H.ppr body

importCG :: Import -> H.Doc
importCG (Import _ fr _ _ _) = H.text $ "import qualified " ++ modprefix fr

importsCG :: [Import] -> H.Doc
importsCG = H.vcat . map importCG

primportsCG :: String -> [Dec] -> H.Doc
primportsCG me ds =
 let importfr :: String -> String
     importfr s = unname (qualification (name s))

     -- Primitive variable imports.
     -- We import fully qualified the module where the haskell primitive is
     -- defined.
     pi :: Dec -> Maybe String
     pi (PrimD _ s _) = Just (importfr s)
     pi _ = Nothing

     pidoc :: String -> H.Doc
     pidoc s = H.text "import qualified" H.<+> H.text s

     piimps = map pidoc (nub $ catMaybes (map pi ds))

     -- Primitive data imports.
     -- We import qualified as the local module just the data type definition
     -- from where it is defined.
     pid :: (Name, String) -> Maybe H.Doc
     pid (nm, str)
       | modprefix (qualification nm) == me = return $
             H.text "import" H.<+> H.text str H.<+> H.text "as" H.<+>
             H.text me H.<+> H.parens (H.text (unname $ unqualified nm))
       | otherwise = Nothing

     pidimps = catMaybes $ map pid primdatas

     -- imports needed for Haskellys
     
     hs :: (Name, String) -> Maybe String
     hs (nm, str)
       | modprefix (qualification nm) == me = return str
       | otherwise = Nothing

     hsdoc :: String -> H.Doc
     hsdoc str = H.text "import qualified" H.<+> H.text str 
     hsimps = map hsdoc (nub $ catMaybes (map hs haskellys))
 in H.vcat (piimps ++ pidimps ++ hsimps)

decsCG :: Env -> [Dec] -> Failable [H.Dec]
decsCG env ds = concat <$> runCG env (mapM decCG ds)

