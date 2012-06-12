
{-# LANGUAGE TemplateHaskell #-}

module Seri.Serif.Serif (serif)
  where


import qualified Language.Haskell.TH as H
import Seri.Lambda
import Seri.Utils.Ppr
import Seri.FrontEnd.Typed
import Seri.Serif.Polymorphic

serif :: [Dec] -> [H.Dec]
serif = concat . map mkdec  

-- mkexp bound e
--  bound - a list of bound names.
mkexp :: [Name] -> Exp -> H.Exp
mkexp _ (IntegerE i)
 = apply 'integerE [H.LitE (H.IntegerL i)]
mkexp _ (PrimE (Sig n _))
 = apply 'primitive [string n]
mkexp bound (CaseE e ms)
 = apply 'caseE [mkexp bound e, H.ListE (map (mkmatch bound) ms)]
mkexp _ (ConE (Sig nm _))
 = apply 'conE [H.VarE (vnm nm), string nm]
mkexp bound (VarE (Sig n _) _) | n `elem` bound
 = H.VarE (mknm n)
mkexp bound (VarE (Sig n _) _) 
 = apply 'dvarE [H.VarE (vnm n), H.VarE (iidnm n), string n]
mkexp bound (AppE a b)
 = apply 'appE [mkexp bound a, mkexp bound b]
mkexp bound (LamE (Sig n _) e)
 = apply 'lamE [string n, H.LamE [H.VarP (mknm n)] (mkexp (n:bound) e)]

mkmatch :: [Name] -> Match -> H.Exp
mkmatch bound (Match p e) = 
  let lamify :: [Name] -> H.Exp -> H.Exp
      lamify [] e = e
      lamify (n:ns) e = lamify ns (apply 'lamM [string n, H.LamE [H.VarP $ vpnm n, H.VarP (mknm n)] e])

      vns = varps p
  in lamify vns (apply 'match [mkpat p, mkexp (vns ++ bound) e])

mknm :: Name -> H.Name
mknm = H.mkName . desymbol

mkty :: Type -> H.Type
mkty (ConT "->") = H.ArrowT
mkty (ConT "()") = H.ConT (H.mkName "()")
mkty (ConT "(,)") = H.TupleT 2
mkty (ConT "(,,)") = H.TupleT 3
mkty (ConT "[]") = H.ListT
mkty (ConT nm) = H.ConT (mknm nm)
mkty (AppT a b) = H.AppT (mkty a) (mkty b)
mkty (VarT n) = H.VarT (mknm n)
mkty t = error $ "TODO: mkty: " ++ render (ppr t)

mkdec :: Dec -> [H.Dec]
mkdec (ValD (Sig n t) e) = declval n t e
mkdec (DataD n vars cs) = decltype n vars cs
mkdec (ClassD n vars sigs) = declclass n vars sigs
mkdec (InstD cls ms) = declinst cls ms

mkpat :: Pat -> H.Exp
mkpat (ConP (Sig n _) ps) =
  let mkpat' :: H.Exp -> [Pat] -> H.Exp
      mkpat' e [] = e
      mkpat' e (p:ps) = mkpat' (apply 'appP [e, mkpat p]) ps
  in mkpat' (apply 'conP [H.VarE (vnm n), string n]) ps
mkpat (VarP (Sig n _)) = H.VarE $ vpnm n
mkpat (IntegerP i) = apply 'integerP [H.LitE (H.IntegerL i)]
mkpat (WildP _) = H.VarE 'wildP

apply :: H.Name -> [H.Exp] -> H.Exp
apply n es = foldl H.AppE (H.VarE n) es

string :: Name -> H.Exp
string n = H.LitE (H.StringL n)

vnm :: Name -> H.Name
vnm n = H.mkName ("_serifP_" ++ (desymbol n))

vpnm :: Name -> H.Name
vpnm n = H.mkName ("p_" ++ n)

clsnm :: Name -> H.Name
clsnm n = H.mkName ("SeriClass_" ++ (desymbol n))

-- Return the haskell type for a value declaration
-- For example:
--  input: forall a . (Ex a) => a -> Integer
--  output: forall a . (Eq a, SeriType a) => Typed Exp (a -> Integer)
vty :: Type -> H.Type
vty (ForallT vns ctx t) =
 let mkpred :: Class -> H.Pred
     mkpred (Class n ts) = H.ClassP (clsnm n) (map mkty ts)

     ctx' = (map mkpred ctx) ++ (map stpred vns)
 in H.ForallT (map (H.PlainTV . mknm) vns) ctx' (texpify t)
vty t = texpify t

iidnm :: Name -> H.Name
iidnm n = H.mkName ("_serifI_" ++ (desymbol n))

iidty :: Type -> H.Type
iidty (ForallT vns _ t) = H.ForallT (map (H.PlainTV . mknm) vns) [] (iidty t)
iidty t = arrowts [texpify t, H.ConT ''VarInfo]

desymbol :: String -> String
desymbol s =
  let syms = [
          ('!', "__bang"), ('#', "__hash"), ('$', "__dollar"),
          ('%', "__percent"), ('&', "__amp"), ('*', "__star"),
          ('+', "__plus"), ('.', "__dot"), ('/', "__slash"),
          ('<', "__lt"), ('=', "__eq"), ('>', "__gt"),
          ('?', "__ques"), ('@', "__at"), ('\\', "__bslash"),
          ('^', "__hat"), ('-', "__minus"), ('(', "__oparen"),
          (',', "__comma"), (')', "__cparen"), (':', "__colon"),
          ('[', "__obracket"), (']', "__cbracket")
          ]
    
      ds :: Char -> String
      ds c = case lookup c syms of
                Just str -> str
                Nothing -> [c]
   in concat . map ds $ s

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

-- decltype n vars cs
--  declares:
--    - a haskell declaration of the data type
--    - data constructor types
--    - instance of SeriType
decltype :: Name -> [Name] -> [Con] -> [H.Dec]
decltype dt vars cs = 
  let mkcon :: Con -> H.Con
      mkcon (Con n ts) = H.NormalC (mknm n) (map (\t -> (H.NotStrict, mkty t)) ts)

      data_D = H.DataD [] (mknm dt) (map (H.PlainTV . mknm) vars) (map mkcon cs) []
    
      dtapp = foldl AppT (ConT dt) (map VarT vars)

      contextify :: Type -> Type
      contextify t = ForallT vars [] t

      contype :: [Type] -> Type
      contype ts = contextify $ arrowsT (ts ++ [dtapp])

      declcon :: Con -> [H.Dec]
      declcon (Con n ts) = 
        let dt = vty (contype ts)
            sig_P = H.SigD (vnm n) dt
            impl_P = H.FunD (vnm n) [H.Clause [] (H.NormalB (apply 'conE' [string n])) []]
        in [sig_P, impl_P]

      stcls = mknm $ kindsuf (toInteger $ length vars) "SeriType"
      stmeth = mknm $ kindsuf (toInteger $ length vars) "seritype"
      stimpl = H.FunD stmeth [H.Clause [H.WildP] (H.NormalB (H.AppE (H.ConE 'ConT) (string dt))) []]
      stinst = H.InstanceD [] (H.AppT (H.ConT stcls) (H.ConT (mknm dt))) [stimpl]
   
  in [data_D, stinst] ++ (concat $ map declcon cs)

-- declval n vars sigs
-- Declare a class.
--  Generated declarations are:
--    class (SeriType vars) => <clsnm n> vars where
--       <vnm sig> :: Typed Exp ty
--       <iidname n> :: Typed Exp ty -> VarInfo
declclass :: Name -> [Name] -> [Sig] -> [H.Dec]
declclass nm vars sigs =
  let mksig :: Sig -> [H.Dec]
      mksig (Sig n t) =
        let sig_P = H.SigD (vnm n) (vty t)
            sig_I = H.SigD (iidnm n) (iidty t)
        in [sig_P, sig_I]

      ctx = map stpred vars
      class_D = H.ClassD ctx (clsnm nm) (map (H.PlainTV . mknm) vars) [] (concat $ map mksig sigs)
  in [class_D]

declinst :: Class -> [Method] -> [H.Dec]
declinst (Class n ts) ms = 
  let mkimpl :: Method -> [H.Dec]
      mkimpl (Method n b) =
        let p = H.ValD (H.VarP (vnm n)) (H.NormalB (mkexp [] b)) []
        in [p]

      impls = concat $ map mkimpl ms
      t = foldl H.AppT (H.ConT $ clsnm n) (map mkty ts)
      inst_D = H.InstanceD [] t impls
  in [inst_D]

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

-- Given a type variable, figure out what predicate we should add for it
-- in the context.
stpred :: Name -> H.Pred
stpred v
 = H.ClassP (mknm (kindsuf (tvarkind v) "SeriType")) [H.VarT (mknm v)]

-- Get the list of variable pattern names in the given pattern.
varps :: Pat -> [Name]
varps (VarP (Sig nm _)) = [nm]
varps (ConP _ ps) = concat (map varps ps)
varps (WildP {}) = []
varps (IntegerP {}) = []
