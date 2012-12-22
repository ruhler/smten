
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Seri.Dec.Ppr () where

import Seri.Ppr
import Seri.Type
import Seri.Dec.Dec

conlist :: [Con] -> Doc
conlist [] = empty
conlist (x:xs) = text " " <+> ppr x
                    $+$ vcat (map (\c -> text "|" <+> ppr c) xs)

instance Ppr TyVar where
    ppr (NormalTV n) = ppr n
    ppr (NumericTV n) = text "#" <> ppr n

instance Ppr Dec where
    ppr (ValD s@(TopSig n _ _) e)
        = ppr s <> semi $$
          fsep [ppr n <+> text "=", nest tabwidth $ ppr e]
    ppr (DataD n vs cs)
        = text "data" <+> ppr n <+> hsep (map ppr vs) <+> text "=" $$
            (nest tabwidth (conlist cs))
    ppr (ClassD n vs ss)
        = text "class" <+> ppr n <+> hsep (map ppr vs)
                <+> text "where" <+> text "{"
                $+$ nest tabwidth (vcat (punctuate semi (map ppr ss))) $+$ text "}"
    ppr (InstD ctx cls ms)
        = text "instance"
                <+> ppr ctx
                <+> ppr cls
                <+> text "where" <+> text "{"
                $+$ nest tabwidth (vcat (map ppr ms)) $+$ text "}"
    ppr (PrimD s) = ppr s

instance Ppr TopSig where
    ppr (TopSig n ctx t)
      = ppr n <+> text "::" <+> ppr ctx <+> ppr t

instance Ppr Context where
    ppr [] = empty
    ppr xs = parens (sep (punctuate comma $ map ppr xs)) <+> text "=>"
    
instance Ppr Con where
    ppr (Con n ts) =
      let pprt t = parens (ppr t)
      in ppr n <+> hsep (map pprt ts)

instance Ppr Method where
    ppr (Method n e)
       = fsep [ppr n <+> text "=", nest tabwidth $ ppr e <> semi]

instance Ppr [Dec] where
    ppr ds = vcat (punctuate semi (map (\d -> ppr d $+$ text "") ds))

instance Ppr Class where
    ppr (Class n ts) = ppr (appsT (ConT n) ts)

