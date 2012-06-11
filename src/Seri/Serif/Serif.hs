
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
mkexp _ (IntegerE i)
 = apply 'integerE [H.LitE (H.IntegerL i)]
mkexp _ (PrimE (Sig n _))
 = apply 'primitive [H.LitE (H.StringL n)]
mkexp bound (VarE (Sig n _) _) | n `elem` bound
 = H.VarE (mknm n)
mkexp bound (VarE (Sig n _) _) 
 = apply 'dvarE [H.VarE (vnm n), H.VarE (iidnm n), string n]
mkexp bound (AppE a b)
 = apply 'appE [mkexp bound a, mkexp bound b]
mkexp bound (LamE (Sig n _) e)
 = apply 'lamE [string n, H.LamE [H.VarP (mknm n)] (mkexp (n:bound) e)]
mkexp _ e = error $ "TODO: mkexp: " ++ render (ppr e)

mknm :: Name -> H.Name
mknm = H.mkName . desymbol

mkty :: Type -> H.Type
mkty (ConT "->") = H.ArrowT
mkty (ConT nm) = H.ConT (mknm nm)
mkty (AppT a b) = H.AppT (mkty a) (mkty b)
mkty t = error $ "TODO: mkty: " ++ render (ppr t)

mkdec :: Dec -> [H.Dec]
mkdec (ValD (Sig n t) e) = declval n t e
mkdec d = error $ "TODO: mkdec: " ++ render (ppr d)

apply :: H.Name -> [H.Exp] -> H.Exp
apply n es = foldl H.AppE (H.VarE n) es

string :: Name -> H.Exp
string n = H.LitE (H.StringL (desymbol n))

vnm :: Name -> H.Name
vnm n = H.mkName ("serifP_" ++ (desymbol n))

-- Return the haskell type for a value declaration
-- For example:
--  input: forall a . (Ex a) => a -> Integer
--  output: forall a . (Eq a, SeriType a) => Typed Exp (a -> Integer)
vty :: Type -> H.Type
vty (ForallT vns ctx t) = error $ "TODO: vty forall"
vty t = texpify t

iidnm :: Name -> H.Name
iidnm n = H.mkName ("serifI_" ++ (desymbol n))

iidty :: Type -> H.Type
iidty (ForallT vns _ t) = H.ForallT (map (H.PlainTV . mknm) vns) [] (iidty t)
iidty t = arrowts [texpify t, H.ConT ''VarInfo]

desymbol :: String -> String
desymbol =
  let syms = [
          ('!', "__bang"), ('#', "__hash"), ('$', "__dollar"),
          ('%', "__percent"), ('&', "__amp"), ('*', "__star"),
          ('+', "__plus"), ('.', "__dot"), ('/', "__slash"),
          ('<', "__lt"), ('=', "__eq"), ('>', "__gt"),
          ('?', "__ques"), ('@', "__at"), ('\\', "__bslash"),
          ('^', "__hat"), ('-', "__minus")
          ]
    
      ds :: Char -> String
      ds c = case lookup c syms of
                Just str -> str
                Nothing -> [c]
   in concat . map ds

-- declval n ty e
-- Declare a value.
--  Generated declarations are:
--    vnm n - The actual declaration:
--       <vnm n> :: Typed Exp ty
--       <vnm n> = e
--    iidname n - The instance id:
--       <iidname n> :: Typed Exp ty -> VarInfo
--       <iidname n> _ = Declared
declval :: Name -> Type -> Exp -> [H.Dec]
declval n ty e = 
  let dt = vty ty
      sig_P = H.SigD (vnm n) dt
      impl_P = H.FunD (vnm n) [H.Clause [] (H.NormalB (mkexp [] e)) []]

      sig_I = H.SigD (iidnm n) (iidty ty)
      impl_I = H.FunD (iidnm n) [H.Clause [H.WildP] (H.NormalB $ H.ConE 'Declared) []]
  in [sig_P, impl_P, sig_I, impl_I]

-- texpify
--   Given type: a
--   Form type : Typed Exp a
texpify :: Type -> H.Type
texpify t = H.AppT (H.AppT (H.ConT ''Typed) (H.ConT ''Exp)) (mkty t)

-- arrowts 
--  Turn a list of types [a, b, c, ...]
--  Into a type (a -> b -> c -> ...)
arrowts :: [H.Type] -> H.Type
arrowts [t] = t
arrowts (x:xs) = H.AppT (H.AppT H.ArrowT x) (arrowts xs)

