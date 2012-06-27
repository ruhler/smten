
module Seri.Target.Haskell.Haskell (
    haskell, haskellH,
    ) where

import Data.Char(isAlphaNum)
import Data.Maybe(fromJust)

import qualified Language.Haskell.TH.PprLib as H
import qualified Language.Haskell.TH as H

import Seri.Failable
import Seri.Lambda
import Seri.Target.Haskell.Compiler
import Seri.Target.Haskell.Builtins.Prelude

hsName :: Name -> H.Name
hsName = H.mkName

hsExp :: HCompiler -> Exp -> Failable H.Exp
hsExp c (IntegerE i) = do
    t <- compile_type c c integerT
    return $ H.SigE (H.LitE (H.IntegerL i)) t
hsExp c (CaseE e ms) = do
    e' <- compile_exp c c e
    ms' <- mapM (hsMatch c) ms
    return $ H.CaseE e' ms'
hsExp c (AppE f x) = do
    f' <- compile_exp c c f
    x' <- compile_exp c c x
    return $ H.AppE f' x'
hsExp c (LamE (Sig n _) x) = do
    x' <- compile_exp c c x
    return $ H.LamE [H.VarP (hsName n)] x'
hsExp c (ConE (Sig n _)) = return $ H.ConE (hsName n)
hsExp c (VarE (Sig n _)) = return $ H.VarE (hsName n)

hsMatch :: HCompiler -> Match -> Failable H.Match
hsMatch c (Match p e) = do
    let p' = hsPat p
    e' <- compile_exp c c e
    return $ H.Match p' (H.NormalB $ e') []
    
hsPat :: Pat -> H.Pat
hsPat (ConP _ n ps) = H.ConP (hsName n) (map hsPat ps)
hsPat (VarP (Sig n _)) = H.VarP (hsName n)
hsPat (IntegerP i) = H.LitP (H.IntegerL i)
hsPat (WildP _) = H.WildP

hsType :: HCompiler -> Type -> Failable H.Type
hsType c (ConT "->") = return H.ArrowT
hsType c (ConT n) = return $ H.ConT (hsName n)
hsType c (AppT a b) = do
    a' <- compile_type c c a
    b' <- compile_type c c b
    return $ H.AppT a' b'
hsType c (VarT n) = return $ H.VarT (hsName n)
hsType c t = fail $ "coreH does not apply to type: " ++ pretty t

hsTopType :: HCompiler -> Context -> Type -> Failable H.Type
hsTopType c [] t = compile_type c c t
hsTopType c ctx t = do
    t' <- compile_type c c t
    ctx' <- mapM (hsClass c) ctx
    return $ H.ForallT (map (H.PlainTV . H.mkName) (varTs t)) ctx' t'

hsClass :: HCompiler -> Class -> Failable H.Pred
hsClass c (Class nm ts) = do
    ts' <- mapM (compile_type c c) ts
    return $ H.ClassP (hsName nm) ts'
    
hsMethod :: HCompiler -> Method -> Failable H.Dec
hsMethod c (Method n e) = do
    let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
    e' <- compile_exp c c e
    return $ H.ValD (H.VarP hsn) (H.NormalB e') []


issymbol :: Name -> Bool
issymbol (h:_) = not $ isAlphaNum h || h == '_'

hsCon :: HCompiler -> Con -> Failable H.Con
hsCon c (Con n tys) = do
    ts <- mapM (compile_type c c) tys
    return $ H.NormalC (hsName n) (map (\t -> (H.NotStrict, t)) ts)
    
hsSig :: HCompiler -> TopSig -> Failable H.Dec
hsSig c (TopSig n ctx t) = do
    let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
    t' <- hsTopType c ctx t
    return $ H.SigD hsn t'

    
hsDec :: HCompiler -> Dec -> Failable [H.Dec]
hsDec c (ValD (TopSig n ctx t) e) = do
    t' <- hsTopType c ctx t
    e' <- compile_exp c c e
    let hsn = hsName $ if issymbol n then "(" ++ n ++ ")" else n
    let sig = H.SigD hsn t'
    let val = H.FunD hsn [H.Clause [] (H.NormalB e') []]
    return [sig, val]

hsDec c (DataD n tyvars constrs) = do
    cs <- mapM (hsCon c) constrs
    return [H.DataD [] (hsName n) (map (H.PlainTV . hsName) tyvars) cs []]

hsDec c (ClassD n vars sigs) = do
    sigs' <- mapM (hsSig c) sigs
    return $ [H.ClassD [] (hsName n) (map (H.PlainTV . hsName) vars) [] sigs']

hsDec c (InstD ctx (Class n ts) ms) = do
    ctx' <- mapM (hsClass c) ctx
    ms' <- mapM (hsMethod c) ms
    ts' <- mapM (compile_type c c) ts
    let t = foldl H.AppT (H.ConT (hsName n)) ts'
    return [H.InstanceD ctx' t ms'] 

hsDec _ d = fail $ "coreH does not apply to dec: " ++ pretty d

coreH :: HCompiler
coreH = Compiler hsExp hsType hsDec

haskellH :: HCompiler
haskellH = compilers [preludeH, coreH]

-- haskell builtin decs
--  Compile the given declarations to haskell.
haskell :: HCompiler -> Env -> Name -> H.Doc
haskell c env main =
  let hsHeader :: H.Doc
      hsHeader = H.text "{-# LANGUAGE ExplicitForAll #-}" H.$+$
                 H.text "{-# LANGUAGE MultiParamTypeClasses #-}" H.$+$
                 H.text "import qualified Prelude"

      ds = compile_decs c env
  in hsHeader H.$+$ H.ppr ds H.$+$
        H.text "main :: Prelude.IO ()" H.$+$
        H.text "main = Prelude.putStrLn (case "
        H.<+> H.text main H.<+> H.text " of { True -> \"True\"; False -> \"False\"})"

