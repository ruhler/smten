
module Seri.Target.Haskell.Haskell (
    haskell
    ) where

import Data.Char(isAlphaNum)
import qualified Language.Haskell.TH as H
import Seri
import Seri.THUtils

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
      hsExp (IntegerE i) = H.LitE (H.IntegerL i)
      hsExp (PrimE _ n) =   
         case mapprim builtin n of  
             Just x -> H.VarE (hsName x)
             Nothing -> error $ "primitive " ++ n ++ " not defined for haskell target"
      hsExp (IfE _ p a b) = H.CondE (hsExp p) (hsExp a) (hsExp b)
      hsExp (CaseE _ e ms) = H.CaseE (hsExp e) (map hsMatch ms)
      hsExp (AppE _ f x) = H.AppE (hsExp f) (hsExp x)
      hsExp (LamE _ n x) = H.LamE [H.VarP (hsName n)] (hsExp x)
      hsExp (ConE _ n) = H.ConE (hsName n)
      hsExp (VarE _ n _) = H.VarE (hsName n)
    
      hsMatch :: Match -> H.Match
      hsMatch (Match p e) = H.Match (hsPat p) (H.NormalB $ hsExp e) []
    
      hsPat :: Pat -> H.Pat
      hsPat = 
        let unfold :: Pat -> [Pat]
            unfold (AppP a b) = unfold a ++ [b]
            unfold p = [p]
    
            foldth :: [Pat] -> H.Pat
            foldth [VarP n] = H.VarP (hsName n)
            foldth [IntegerP i] = H.LitP (H.IntegerL i)
            foldth [WildP] = H.WildP
            foldth ((ConP n):args) = H.ConP (hsName n) (map hsPat args)
        in foldth . unfold
    
      isinfix :: Name -> Bool
      isinfix (h:_) = not $ isAlphaNum h || h == '_'
    
      hsDec :: Dec -> [H.Dec]
      hsDec (ValD n t e) =
        let hsn = hsName $ if isinfix n then "(" ++ n ++ ")" else n
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
      hsSig (Sig n t) = H.SigD (hsName n) (hsType t)

      hsMethod :: Method -> H.Dec
      hsMethod (Method n e) = H.ValD (H.VarP (hsName n)) (H.NormalB (hsExp e)) []

      hsCon :: Con -> H.Con
      hsCon (Con n tys) = H.NormalC (hsName n) (map (\t -> (H.NotStrict, hsType t)) tys)
    
      hsType :: Type -> H.Type
      hsType (ConT "->") = H.ArrowT
      hsType (ConT n) =
         case maptype builtin n of
            Just n' -> H.ConT (hsName n')
            Nothing -> H.ConT (hsName n)
      hsType (AppT a b) = H.AppT (hsType a) (hsType b)
      hsType (VarT n) = H.VarT (hsName n)
      hsType (ForallT vars pred t) =
        H.ForallT (map (H.PlainTV . hsName) vars) (map hsPred pred) (hsType t)

      hsPred :: Pred -> H.Pred
      hsPred (Pred nm [t]) = H.ClassP (hsName nm) [hsType t]
    
      hsHeader :: Doc
      hsHeader = text "{-# LANGUAGE ExplicitForAll #-}" $+$
                 text "import qualified Prelude"

      ds = concat $ map hsDec (decls e)
      me = hsExp $ val e

  in hsHeader $+$
     includes builtin $+$
     ppr ds $+$
     main (ppr me)

