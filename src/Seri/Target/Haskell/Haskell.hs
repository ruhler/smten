
module Seri.Target.Haskell.Haskell (
    haskell
    ) where

import Data.Char(isAlphaNum)
import qualified Language.Haskell.TH as H
import Seri

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
      hsExp (VarE _ n) = H.VarE (hsName n)
    
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
    
      hsType :: Type -> H.Type
      hsType (ConT n) =
         case maptype builtin n of
            Just n' -> H.ConT (hsName n')
            Nothing -> H.ConT (hsName n)
      hsType ArrowT = H.ArrowT
      hsType (AppT a b) = H.AppT (hsType a) (hsType b)
      hsType (VarT n) = H.VarT (hsName n)
    
      hsHeader :: Doc
      hsHeader = text "import qualified Prelude"

      ds = concat $ map hsDec (decls e)
      me = hsExp $ val e

  in hsHeader $+$
     includes builtin $+$
     ppr ds $+$
     main (ppr me)

