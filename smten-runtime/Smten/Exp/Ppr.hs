
{-# LANGUAGE PatternGuards #-}

module Smten.Exp.Ppr () where

import Smten.Ppr
import Smten.Exp.Exp
import Smten.Exp.Sugar

-- Print this expression atomically.
atom :: Exp -> Doc
atom e
 | Just _ <- de_listE e = ppr e
 | Just _ <- de_tupleE e = ppr e
 | AppE {} <- e = parens (ppr e)
 | LamE {} <- e = parens (ppr e)
 | CaseE {} <- e = parens (ppr e)
 | LetE {} <- e = parens (ppr e)
 | otherwise = ppr e

instance Ppr Exp where
    ppr e
      | Just s <- de_stringE e = text (show s)
      | Just xs <- de_listE e
        = text "[" <> sep (punctuate comma (map ppr xs)) <> text "]"
      | Just xs <- de_tupleE e  
        = text "(" <> sep (punctuate comma (map ppr xs)) <> text ")"
      | (ss@(_:_), x) <- de_lamsE e = fsep [
            text "\\" <> sep (map ppr ss) <+> text "->",
            nest tabwidth (ppr x)
          ]
      | (f, xs@(_:_)) <- de_appsE e = sep (map atom (f:xs))

    ppr (LitE _ l) = ppr l
    ppr (ConE _ s) = ppr s
    ppr (VarE _ s) = ppr s
    ppr (CaseE _ x k y n)
        = text "case" <+> atom x <+> text "of" <+> text "{"
            $+$ nest tabwidth (vcat [
                    ppr k <+> text "->" <+> ppr y,
                    text "_" <+> text "->" <+> ppr n
                 ]) $+$ text "}"
    ppr (LetE _ bs x) =
      let f (s, v) = ppr s <+> text "=" <+> ppr v
      in text "let" <+> text "{"
                $+$ nest tabwidth (vcat (map f bs))
            $+$ text "}" <+> text "in" <+> ppr x

