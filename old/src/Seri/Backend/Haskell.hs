
module Seri.Backend.Haskell (haskell)
  where

import qualified Seri.IR as S
import Language.Haskell.TH

haskell :: S.Exp -> Exp
haskell (S.IntegerE i) = LitE (IntegerL i)
haskell (S.AddE a b) = AppE (AppE (VarE $ mkName "+") (haskell a)) (haskell b)
haskell (S.MulE a b) = AppE (AppE (VarE $ mkName "*") (haskell a)) (haskell b)
haskell (S.AppE _ a b) = AppE (haskell a) (haskell b)
haskell (S.LamE _ n e) = LamE [VarP $ mkName n] (haskell e)
haskell (S.VarE _ n) = VarE $ mkName n

