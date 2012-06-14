
{-# LANGUAGE TemplateHaskell #-}

module Seri.Serif.Serif (serif)
  where

import Data.List(transpose)

import qualified Language.Haskell.TH as H

import Seri.Lambda.IR
import Seri.Lambda.Types
import Seri.Serif.Typed
import Seri.Serif.Names
import Seri.Serif.Polymorphic

-- | Given a list of seri declarations, generate a list of haskell
-- declarations which include the declaration of a haskell value
-- 'declarations' of type [Dec] with type inferred and type checked versions
-- of the given seri declarations.
serif :: [Dec] -> [H.Dec]
serif ds =
  let sig = H.SigD (H.mkName "declarations") (H.AppT H.ListT (H.ConT ''Dec))
      impl = H.FunD (H.mkName "declarations") [
                H.Clause [] (H.NormalB (H.ListE (map mksdec ds))) []]
  in library ++ concat (map mkdec ds) ++ [sig, impl] 

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

istuplecon :: Name -> Bool
istuplecon n@('(':_:_) = last n == ')' && all (== ',') (tail (init n))
istuplecon _ = False
 

mkty :: Type -> H.Type
mkty (ConT "->") = H.ArrowT
mkty (ConT "()") = H.ConT (H.mkName "()")
mkty (ConT c) | istuplecon c = H.TupleT (length c - 1)
mkty (ConT "[]") = H.ListT
mkty (ConT nm) = H.ConT (mknm nm)
mkty (AppT a b) = H.AppT (mkty a) (mkty b)
mkty (VarT n) = H.VarT (mknm n)
mkty (ForallT vars ctx t) =
  let mkpred :: Class -> H.Pred
      mkpred (Class n ts) = H.ClassP (clsnm n) (map mkty ts)
  in H.ForallT (map (H.PlainTV . mknm) vars) (map mkpred ctx) (mkty t)

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

applyC :: H.Name -> [H.Exp] -> H.Exp
applyC n es = foldl H.AppE (H.ConE n) es

string :: Name -> H.Exp
string n = H.LitE (H.StringL n)

-- Return the type for a value declaration
-- For example:
--  input: forall a . (Ex a) => a -> Integer
--  output: forall a . (SeriClass_Eq a, SeriType a) => Typed Exp (a -> Integer)
vty :: Type -> H.Type
vty (ForallT vns ctx t) =
 let mkpred :: Class -> H.Pred
     mkpred (Class n ts) = H.ClassP (clsnm n) (map mkty ts)

     ctx' = (map mkpred ctx) ++ (map stpred vns)
 in H.ForallT (map (H.PlainTV . mknm) vns) ctx' (mkty $ texpify t)
vty t = mkty $ texpify t

iidty :: Type -> H.Type
iidty (ForallT vns _ t) = H.ForallT (map (H.PlainTV . mknm) vns) [] (iidty t)
iidty t = arrowts [mkty $ texpify t, H.ConT ''VarInfo]

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

  in [data_D] ++ (declprimtype dt vars cs)

-- Declare a data type without declaring the haskell version.
--  Just the instance of SeriType and the constructors.
declprimtype :: Name -> [Name] -> [Con] -> [H.Dec]
declprimtype dt vars cs = 
  let dtapp = foldl AppT (ConT dt) (map VarT vars)

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

      stinst = decltycon (toInteger $ length vars) dt
   
  in stinst ++ (concat $ map declcon cs)

decltycon :: Integer -> Name -> [H.Dec]
decltycon k n =
  let stcls = mknm $ kindsuf k "SeriType"
      stmeth = mknm $ kindsuf k "seritype"
      stimpl = H.FunD stmeth [H.Clause [H.WildP] (H.NormalB (H.AppE (H.ConE 'ConT) (string n))) []]
      stinst = H.InstanceD [] (H.AppT (H.ConT stcls) (H.ConT (H.mkName n))) [stimpl]

      body = applyC 'ConT [string n]
      sig_S = H.SigD (tyctnm n) (H.ConT ''Type)
      impl_S = H.ValD (H.VarP (tyctnm n)) (H.NormalB body) []

      vars = replicate (fromInteger k) (H.ConT (H.mkName "()"))
      sig_K = H.SigD (cvnm n) (foldl H.AppT (H.ConT $ mknm n) vars)
      impl_K = H.ValD (H.VarP (cvnm n)) (H.NormalB (H.VarE (H.mkName "undefined"))) []

  in [stinst, sig_S, impl_S, sig_K, impl_K]

decltyvar :: Name -> [H.Dec]
decltyvar vn =
  let nm = "VarT_" ++ vn
      k = tvarkind vn
      vars = map (\n -> H.PlainTV (mknm [n])) (take (fromInteger k) "abcd")
      dataD = H.DataD [] (mknm nm) vars [H.NormalC (mknm nm) []] []

      classname = mknm $ kindsuf k "SeriType"
      methname = mknm $ kindsuf k "seritype"

      polytype = mkty (concrete (VarT vn))

      body = applyC 'VarT [string vn]
      stimpl = H.FunD methname [H.Clause [H.WildP] (H.NormalB body ) []]
      stinst = H.InstanceD [] (H.AppT (H.ConT classname) polytype) [stimpl]

      sig_S = H.SigD (tyctnm nm) (H.ConT ''Type)
      impl_S = H.ValD (H.VarP (tyctnm nm)) (H.NormalB body) []
  in [dataD, stinst, sig_S, impl_S]

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

      mkt :: Sig -> [H.Dec]
      mkt (Sig n t) = 
        let vararg :: Name -> Type
            vararg v = appsT $ [VarT v] ++ replicate (fromInteger $ tvarkind v) (ConT "()")
        
            ty = mkty . arrowsT $ map vararg vars ++ [texpify (concrete' vars t)]
            sig_T = H.SigD (mtynm n) (H.ForallT (map (H.PlainTV . mknm) vars) [] ty)
            impl_T = H.FunD (mtynm n) [H.Clause [H.WildP] (H.NormalB (H.VarE (H.mkName "undefined"))) []]
        in [sig_T, impl_T]

      type_ds = concat $ map mkt sigs

      mkvartinst :: [Name] -> [H.Dec]
      mkvartinst varts =
        let mkmeth :: Sig -> (Name, H.Exp)
            mkmeth (Sig n _) = (n, H.VarE $ H.mkName "undefined")
        in hdeclinst (Class nm (map (concrete . VarT) varts)) (map mkmeth sigs)

      product :: [[a]] -> [[a]]
      product [x] = [[y] | y <- x]
      product (x:xs) = [y : p | y <- x, p <- product xs]

      varts :: [[Name]]
      varts = product $ map (tyvarsk . tvarkind) vars
      vartinsts = map mkvartinst varts

  in [class_D] ++ type_ds ++ concat vartinsts

declinst :: Class -> [Method] -> [H.Dec]
declinst cls@(Class n ts) ms = 
  let mkimpl :: Method -> (Name, H.Exp)
      mkimpl (Method n b) = (n, mkexp [] b)
  in hdeclinst cls (map mkimpl ms)

hdeclinst :: Class -> [(Name, H.Exp)] -> [H.Dec]
hdeclinst (Class n ts) ms =
  let itys = H.ListE $ map seritypeexp ts

      mkimpl :: (Name, H.Exp) -> [H.Dec]
      mkimpl (mn, e) =
        let i = H.FunD (iidnm mn) [H.Clause [H.WildP] (H.NormalB (applyC 'Instance [applyC 'Class [string n, itys]])) []]
            p = H.ValD (H.VarP (vnm mn)) (H.NormalB e) []
        in [p, i]

      impls = concat $ map mkimpl ms
      t = foldl H.AppT (H.ConT $ clsnm n) (map mkty ts)
      inst_D = H.InstanceD [] t impls
  in [inst_D]


-- texpify
--   Given type: a
--   Form type : Typed Exp a
texpify :: Type -> Type
texpify t = AppT (AppT (ConT "Typed") (ConT "Exp")) t

-- arrowts 
--  Turn a list of types [a, b, c, ...]
--  Into a type (a -> b -> c -> ...)
arrowts :: [H.Type] -> H.Type
arrowts [t] = t
arrowts (x:xs) = H.AppT (H.AppT H.ArrowT x) (arrowts xs)

-- Given a type variable, figure out what predicate we should add for it
-- in the context.
stpred :: Name -> H.Pred
stpred v = H.ClassP (mknm $ kindsuf (tvarkind v) "SeriType") [H.VarT (mknm v)]

-- Get the list of variable pattern names in the given pattern.
varps :: Pat -> [Name]
varps (VarP (Sig nm _)) = [nm]
varps (ConP _ ps) = concat (map varps ps)
varps (WildP {}) = []
varps (IntegerP {}) = []

-- Given a seri declaration, return a haskell expression representing that
-- seri declaration with types inferred.
mksdec :: Dec -> H.Exp
mksdec (ValD (Sig n t) e) =
  let e' = apply 'typed [H.SigE (H.VarE (vnm n)) (hconcrete (vty t))]
  in applyC 'ValD [applyC 'Sig [string n, seritypeexp t], e']
mksdec (DataD dt vars cs) =
  let mkscon (Con n ts) = applyC 'Con [string n, H.ListE (map seritypeexp ts)]
  in applyC 'DataD [string dt, H.ListE (map string vars), H.ListE (map mkscon cs)]
mksdec (ClassD n vars sigs) = 
  let mkdsig (Sig n t) = applyC 'Sig [string n, seritypeexp t]
      tyvars = H.ListE (map string vars)
      dsigs = H.ListE (map mkdsig sigs)
  in applyC 'ClassD [string n, tyvars, dsigs]
mksdec (InstD (Class n ts) ms) =
  let cvals = map (\(ConT tnm) -> H.VarE (cvnm tnm)) ts
      mkmeth (Method n e) = apply 'method [string n, apply (mtynm n) cvals, mkexp [] e]
      ts' = H.ListE (map seritypeexp ts)
      ms' = H.ListE (map mkmeth ms)
  in applyC 'InstD [applyC 'Class [string n, ts'], ms']


-- Given a type, return an expression corresonding to the seri type of
-- that type.
seritypeexp :: Type -> H.Exp
seritypeexp (VarT nm) = applyC 'VarT [string nm]
seritypeexp (ForallT vars preds t) =
 let vars' = H.ListE $ map string vars

     mkpred :: Class -> H.Exp
     mkpred (Class n ts) =
        applyC 'Class [string n, H.ListE $ map seritypeexp ts]

     preds' = H.ListE $ map mkpred preds
 in applyC 'ForallT [vars', preds', seritypeexp t]
seritypeexp (ConT nm) = H.VarE (tyctnm nm)
seritypeexp t = apply 'seritype [H.SigE (H.VarE $ H.mkName "undefined") (mkty $ concrete t)]

decltuple :: Int -> [H.Dec]
decltuple n = 
  let nm = "(" ++ replicate (n-1) ',' ++ ")"
      vars = [[c] | c <- take n "abcdefghi"]
  in declprimtype nm vars [Con nm (map VarT vars)]

library :: [H.Dec]
library = concat [decltycon 0 "Integer",
                  decltycon 0 "Char",
                  declprimtype "Bool" [] [Con "True" [], Con "False" []],
                  declprimtype "()" [] [Con "()" []],
                  decltuple 2,
                  decltuple 3,
                  decltuple 4,
                  declprimtype "[]" ["a"] [
                    Con "[]" [],
                    Con ":" [VarT "a", AppT (ConT "[]") (VarT "a")]],
                  concat $ map decltyvar (concat (map snd tyvars))
                  ]


