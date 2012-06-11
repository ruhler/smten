
{-# LANGUAGE TemplateHaskell #-}

module Seri.Serif.Serif (serif)
  where


import qualified Language.Haskell.TH as H
import Seri.Lambda
import Seri.Utils.Ppr
import Seri.FrontEnd.Typed

serif :: [Dec] -> [H.Dec]
serif = concat . map mkdec  

-- mkexp bound e
--  bound - a list of bound names.
mkexp :: [Name] -> Exp -> H.Exp
mkexp bound (VarE (Sig n _) _) | n `elem` bound
 = H.VarE (mknm n)
mkexp bound (VarE (Sig n _) _) 
 = apply 'dvarE [H.VarE (vnm n), H.VarE (iidnm n), string n]
mkexp _ (IntegerE i)
 = apply 'integerE [H.LitE (H.IntegerL i)]
mkexp bound (AppE a b)
 = apply 'appE [mkexp bound a, mkexp bound b]
mkexp bound (LamE (Sig n _) e)
 = apply 'lamE [string n, H.LamE [H.VarP (mknm n)] (mkexp (n:bound) e)]
mkexp _ e = error $ "TODO: mkexp: " ++ render (ppr e)

mknm :: Name -> H.Name
mknm = H.mkName

mkty :: Type -> H.Type
mkty (ConT nm) = H.ConT (mknm nm)
mkty t = error $ "TODO: mkty: " ++ render (ppr t)

mkdec :: Dec -> [H.Dec]
mkdec (ValD (Sig n t) e)
 = [H.SigD (mknm n) (mkty t), H.ValD (H.VarP (mknm n)) (H.NormalB (mkexp [] e)) []]
mkdec d = error $ "TODO: mkdec: " ++ render (ppr d)

apply :: H.Name -> [H.Exp] -> H.Exp
apply n es = foldl H.AppE (H.VarE n) es

string :: Name -> H.Exp
string n = H.LitE (H.StringL n)

vnm :: Name -> H.Name
vnm n = H.mkName ("serifP_" ++ n)

iidnm :: Name -> H.Name
iidnm n = H.mkName ("serifI_" ++ n)

