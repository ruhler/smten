
module Seri.Target.Haskell.Haskell (
    haskell
    ) where

import Data.Char(isAlphaNum)
import Data.Maybe(fromJust)

import qualified Language.Haskell.TH as H
import Seri.Lambda as Seri
import Seri.Lib.Prelude
import Seri.Utils.TH
import Seri.Utils.Ppr

import Seri.Target.Haskell.Builtin

-- haskell builtin main exp
--  Compile the given expression and its environment to haskell.
--
--  builtin - a description of builtin things.
--  main - A function to generate the main function in the haskell code.
--         The input to the function is the haskell text for the compiled
--         expression.
--  exp - The expression to compile to haskell.
haskell :: Builtin -> (Doc -> Doc) -> Env Exp -> Doc
haskell builtin main e =
  let hsName :: Name -> H.Name
      hsName = H.mkName
    
      hsExp :: Seri.Exp -> H.Exp
      hsExp e | mapexp builtin e /= Nothing = fromJust (mapexp builtin e)
      hsExp (IntegerE i) = H.SigE (H.LitE (H.IntegerL i)) (hsType (ConT "Integer"))
      hsExp (PrimE (Sig n _)) = error $ "primitive " ++ n ++ " not defined for haskell target"
      hsExp (CaseE e ms) = H.CaseE (hsExp e) (map hsMatch ms)
      hsExp (AppE f x) = H.AppE (hsExp f) (hsExp x)
      hsExp (LamE (Sig n _) x) = H.LamE [H.VarP (hsName n)] (hsExp x)
      hsExp (ConE (Sig n _)) = H.ConE (hsName n)
      hsExp (VarE (Sig n _) _) = H.VarE (hsName n)
    
      hsMatch :: Match -> H.Match
      hsMatch (Match p e) = H.Match (hsPat p) (H.NormalB $ hsExp e) []
    
      hsPat :: Pat -> H.Pat
      hsPat (ConP (Sig n _) ps) = H.ConP (hsName n) (map hsPat ps)
      hsPat (VarP (Sig n _)) = H.VarP (hsName n)
      hsPat (IntegerP i) = H.LitP (H.IntegerL i)
      hsPat (WildP _) = H.WildP
         
      issymbol :: Name -> Bool
      issymbol (h:_) = not $ isAlphaNum h || h == '_'
    
      hsDec :: Dec -> [H.Dec]
      hsDec (ValD (Sig n t) e) =
        let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
            sig = H.SigD hsn (hsType t)
            val = H.FunD hsn [H.Clause [] (H.NormalB (hsExp e)) []]
        in [sig, val]

      hsDec (DataD n tyvars constrs)    
        = [H.DataD [] (hsName n) (map (H.PlainTV . hsName) tyvars) (map hsCon constrs) []]

      hsDec (ClassD n vars sigs)
        = [H.ClassD [] (hsName n) (map (H.PlainTV . hsName) vars) [] (map hsSig sigs)]

      hsDec (InstD n ts ms)
        = [H.InstanceD [] (appts ((H.ConT (hsName n)):(map hsType ts))) (map hsMethod ms)] 

      hsSig :: Sig -> H.Dec
      hsSig (Sig n t) =
        let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
        in H.SigD hsn (hsType t)

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
      hsType (ForallT vars pred t) =
        H.ForallT (map (H.PlainTV . hsName) vars) (map hsPred pred) (hsType t)

      hsPred :: Pred -> H.Pred
      hsPred (Pred nm ts) = H.ClassP (hsName nm) (map hsType ts)
    
      hsHeader :: Doc
      hsHeader = text "{-# LANGUAGE ExplicitForAll #-}" $+$
                 text "{-# LANGUAGE MultiParamTypeClasses #-}" $+$
                 text "import qualified Prelude"

      ds = concat $ map hsDec (decls e)
      me = hsExp $ val e

  in hsHeader $+$
     includes builtin $+$
     ppr ds $+$
     main (ppr me)
