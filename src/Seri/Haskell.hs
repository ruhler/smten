
module Seri.Haskell (
    Module(..),
    Import(..), ImportSpec(..), ImportItem(..),
    Export(..),
    module Language.Haskell.TH,
    ) where

import Data.Maybe

import Language.Haskell.TH
import Language.Haskell.TH.PprLib

data Export = ExportVar Name
            | ExportMod Name
    deriving(Show, Eq)

instance Ppr Export where
    ppr (ExportVar n) = ppr n
    ppr (ExportMod n) = text "module" <+> ppr n

pprexports :: [Export] -> Doc
pprexports es = parens (sep (punctuate comma (map ppr es)))
    

data ImportItem = ImportItemVar Name
    deriving(Show, Eq)

instance Ppr ImportItem where
    ppr (ImportItemVar n) = ppr n

data ImportSpec = Importing [ImportItem]
                | Hiding [ImportItem]
                | EmptySpec
    deriving(Show, Eq)

instance Ppr ImportSpec where
    ppr (Importing items) = parens (sep (punctuate comma (map ppr items)))
    ppr (Hiding items) = text "hiding" <+> 
        parens (sep (punctuate comma (map ppr items)))
    ppr EmptySpec = empty


-- | import [qualified] modid [as modid] spec
data Import = Import Bool Name (Maybe Name) ImportSpec
    deriving(Show, Eq)

instance Ppr Import where
    ppr (Import q n as spec)
     = text "import"
        <+> (if q then text "qualified" else empty)
        <+> ppr n
        <+> fromMaybe empty (as >>= return . ppr)
        <+> ppr spec

data Module = Module Name (Maybe [Export]) [Import] [Dec]
    deriving(Show, Eq)

instance Ppr Module where
    ppr (Module n es is ds) =
      text "module" <+> ppr n
            <+> fromMaybe empty (es >>= return . pprexports)
            <+> text "where"
            $+$ vcat (map ppr is)
            $+$ ppr ds
            
