
{-# LANGUAGE FlexibleInstances #-}

module Smten.Module.Ppr () where

import Smten.Ppr
import Smten.Module.Module

instance Ppr ImportSpec where
    ppr (Include ns) = parens $
        sep $ punctuate comma (map ppr ns)
    ppr (Exclude ns) = text "hiding" <+> (
        parens $ sep $ punctuate comma (map ppr ns))

instance Ppr Export where
    ppr (EntityExport n) = ppr n
    ppr (ModuleExport n) = text "module" <+> ppr n

instance Ppr Exports where
    ppr Local = empty
    ppr (Exports es) = sep $ punctuate comma (map ppr es)

instance Ppr Import where
    ppr (Import f a p spec) =
      let as = if (f == a)
                  then empty
                  else text "as" <+> ppr a
          qf = if p 
                  then text "qualified"
                  else empty
      in text "import" <+> qf <+> ppr f <+> as <+> ppr spec <> semi

instance Ppr Synonym where
    ppr (Synonym n vs t)
      = sep ([text "type", ppr n] 
                ++ map ppr vs
                ++ [text "=", ppr t]) <> semi

instance Ppr Deriving where
    ppr (Deriving _ ctx cls) = sep [
            text "deriving",
            text "instance",
            ppr ctx,
            ppr cls] <> semi

instance Ppr Module where
    ppr m
        = text "module" <+> ppr (mod_name m) <+> ppr (mod_exports m) <+> text "where" <+> text "{"
            $+$ nest tabwidth (
                vcat (map ppr (mod_imports m))
                $+$ vcat (map ppr (mod_synonyms m))
                $+$ ppr (mod_decs m)) $+$ text "}"

instance Ppr [Module] where
    ppr ms = vcat (map ppr ms)

