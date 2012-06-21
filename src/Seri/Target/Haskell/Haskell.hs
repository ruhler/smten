
module Seri.Target.Haskell.Haskell (
    haskell
    ) where

import Data.Char(isAlphaNum)
import Data.Maybe(fromJust)

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Lambda as Seri
import Seri.Target.Haskell.Builtin

-- haskell builtin decs
--  Compile the given declarations to haskell.
haskell :: Builtin -> Env Name -> H.Doc
haskell builtin main =
  let hsName :: Name -> H.Name
      hsName = H.mkName
    
      hsExp :: Seri.Exp -> H.Exp
      hsExp e | mapexp builtin e /= Nothing = fromJust (mapexp builtin e)
      hsExp (IntegerE i) = H.SigE (H.LitE (H.IntegerL i)) (hsType (ConT "Integer"))
      hsExp (CaseE e ms) = H.CaseE (hsExp e) (map hsMatch ms)
      hsExp (AppE f x) = H.AppE (hsExp f) (hsExp x)
      hsExp (LamE (Sig n _) x) = H.LamE [H.VarP (hsName n)] (hsExp x)
      hsExp (ConE (Sig n _)) = H.ConE (hsName n)
      hsExp (VarE (Sig n _)) = H.VarE (hsName n)
    
      hsMatch :: Match -> H.Match
      hsMatch (Match p e) = H.Match (hsPat p) (H.NormalB $ hsExp e) []
    
      hsPat :: Pat -> H.Pat
      hsPat (ConP _ n ps) = H.ConP (hsName n) (map hsPat ps)
      hsPat (VarP (Sig n _)) = H.VarP (hsName n)
      hsPat (IntegerP i) = H.LitP (H.IntegerL i)
      hsPat (WildP _) = H.WildP
         
      issymbol :: Name -> Bool
      issymbol (h:_) = not $ isAlphaNum h || h == '_'
    
      hsDec :: Dec -> [H.Dec]
      hsDec (ValD (TopSig n c t) e) =
        let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
            sig = H.SigD hsn (hsTopType c t)
            val = H.FunD hsn [H.Clause [] (H.NormalB (hsExp e)) []]
        in [sig, val]

      hsDec (DataD n tyvars constrs)    
        = [H.DataD [] (hsName n) (map (H.PlainTV . hsName) tyvars) (map hsCon constrs) []]

      hsDec (ClassD n vars sigs)
        = [H.ClassD [] (hsName n) (map (H.PlainTV . hsName) vars) [] (map hsSig sigs)]

      hsDec (InstD (Class n ts) ms)
        = [H.InstanceD [] (foldl H.AppT (H.ConT (hsName n)) (map hsType ts)) (map hsMethod ms)] 

      hsDec (PrimD {}) = []

      hsSig :: TopSig -> H.Dec
      hsSig (TopSig n c t) =
        let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
        in H.SigD hsn (hsTopType c t)

      hsMethod :: Method -> H.Dec
      hsMethod (Method n e) =
        let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
        in H.ValD (H.VarP hsn) (H.NormalB (hsExp e)) []

      hsCon :: Con -> H.Con
      hsCon (Con n tys) = H.NormalC (hsName n) (map (\t -> (H.NotStrict, hsType t)) tys)
    
      hsType :: Type -> H.Type
      hsType t | maptype builtin t /= Nothing = fromJust (maptype builtin t)
      hsType (ConT "->") = H.ArrowT
      hsType (ConT n) = H.ConT (hsName n)
      hsType (AppT a b) = H.AppT (hsType a) (hsType b)
      hsType (VarT n) = H.VarT (hsName n)

      hsTopType :: Context -> Type -> H.Type
      hsTopType [] t = hsType t
      hsTopType ctx t = H.ForallT (map (H.PlainTV . H.mkName) (varTs t)) (map hsClass ctx) (hsType t)

      hsClass :: Class -> H.Pred
      hsClass (Class nm ts) = H.ClassP (hsName nm) (map hsType ts)
    
      hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "import qualified Prelude"

      ds = concat $ map hsDec (decls main)

  in hsHeader H.$+$ includes builtin H.$+$ H.ppr ds H.$+$
        H.text "main :: Prelude.IO ()" H.$+$
        H.text "main = Prelude.putStrLn (Prelude.show ("
        H.<+> H.text (val main) H.<+> H.text "))"

