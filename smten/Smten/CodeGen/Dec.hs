
module Smten.CodeGen.Dec (decCG) where

import qualified Language.Haskell.TH as H
import Data.Functor((<$>))

import Smten.Name
import Smten.Dec
import Smten.CodeGen.CG
import Smten.CodeGen.Data
import Smten.CodeGen.Exp
import Smten.CodeGen.Name
import Smten.CodeGen.Type

decCG :: Dec -> CG [H.Dec]
decCG (DataD _ n tyvars constrs)
  | n == arrowN = return []
  | n == ioN = primDataCG "Prelude.IO" n tyvars 
  | n == charN = primDataCG "Prelude.Char" n tyvars 
  | otherwise = dataCG n tyvars constrs
decCG (ClassD _ ctx n vars exps) = do
    ctx' <- mapM classCG ctx
    let vars' = map (H.PlainTV . nameCG . tyVarName) vars
    exps' <- local (\s -> s { cg_tyvars = map tyVarName vars ++ cg_tyvars s }) $
        concat <$> mapM topExpCG exps
    return [H.ClassD ctx' (tynameCG n) vars' [] exps']
decCG (InstD _ ctx cls@(Class n ts) ms) = do
    ctx' <- mapM classCG ctx
    ts' <- mapM typeCG ts
    ms' <- concat <$> mapM (methodCG cls) ms
    let t = foldl H.AppT (H.ConT (qtynameCG n)) ts'
    return [H.InstanceD ctx' t ms']
decCG (ValD _ e@(TopExp (TopSig n _ _) _)) = do
    decs <- topExpCG e
    main <- if unqualified n == name "main"
                 then mainCG n
                 else return []
    return $ decs ++ main

decCG (PrimD _ hsnm ts@(TopSig n ctx t))
  | n == name "Prelude.putChar" = do
      sig <- topSigCG ts
      let -- putChar (Char x) = IO ((Prelude.>>) (Prelude.putChar x) (Prelude.return Unit__))
          pat = [H.ConP (qtynameCG charN) [H.VarP (H.mkName "x")]]
          bind = foldl1 H.AppE [
                H.VarE $ H.mkName "(Prelude.>>)",
                H.AppE (H.VarE $ H.mkName "Prelude.putChar") (H.VarE (H.mkName "x")),
                H.AppE (H.VarE $ H.mkName "Prelude.return") (H.ConE $ qnameCG unitN)
                ]
          body = H.AppE (H.ConE (qtynameCG ioN)) bind
          clause = H.Clause pat (H.NormalB body) []
          fun = H.FunD (nameCG n) [clause]
      return [sig, fun]

  | n == name "Prelude.error" = do
      sig <- topSigCG ts
      let -- error = Prelude.error "smten error"
          body = H.AppE (H.VarE $ H.mkName "Prelude.error")
                        (H.LitE (H.StringL "smten error"))
          clause = H.Clause [] (H.NormalB body) []
          fun = H.FunD (nameCG n) [clause]
      return [sig, fun]

  | n == name "Prelude.return_io" = do
      sig <- topSigCG ts
      let -- return_io x = IO (Prelude.return x)
          pat = [H.VarP (H.mkName "x")]
          body = H.AppE (H.ConE (qtynameCG ioN))
                        (H.AppE (H.VarE $ H.mkName "Prelude.return") (H.VarE (H.mkName "x")))
          clause = H.Clause pat (H.NormalB body) []
          fun = H.FunD (nameCG n) [clause]
      return [sig, fun]

  | n == name "Prelude.bind_io" = do
      sig <- topSigCG ts
      let -- bind_io (IO x) f =
          --    IO ((Prelude.>>=) x (\v -> let IO r = f v in r))
          pat = [H.ConP (qtynameCG ioN) [H.VarP (H.mkName "x")], H.VarP (H.mkName "f")]
          bind = foldl1 H.AppE [
                    H.VarE $ H.mkName "(Prelude.>>=)",
                    H.VarE $ H.mkName "x",
                    H.LamE [H.VarP $ H.mkName "v"] $
                        H.LetE [H.ValD (H.ConP (qtynameCG ioN) [H.VarP $ H.mkName "r"]) (H.NormalB $ H.AppE (H.VarE (H.mkName "f")) (H.VarE (H.mkName "v"))) []] (H.VarE $ H.mkName "r")
                  ]
          body = H.AppE (H.ConE (qtynameCG ioN)) bind
          clause = H.Clause pat (H.NormalB body) []
          fun = H.FunD (nameCG n) [clause]
      return [sig, fun]
     
  | otherwise = error $ "TODO: decCG prim " ++ unname n

methodCG :: Class -> Method -> CG [H.Dec]
methodCG cls (Method n e) = do
    env <- asks cg_env
    mt <- lookupMethodType env n cls
    mctx <- lookupMethodContext env n cls
    topExpCG (TopExp (TopSig n mctx mt) e)

mainCG :: Name -> CG [H.Dec]
mainCG n = do
  let -- main__ =
      --   let IO x = main
      --   in (Prelude.>>) x (Prelude.return ())
     pat = H.ConP (qtynameCG ioN) [H.VarP (H.mkName "x")]
     bind = foldl1 H.AppE [
           H.VarE $ H.mkName "(Prelude.>>)",
           H.VarE $ H.mkName "x",
           H.AppE (H.VarE $ H.mkName "Prelude.return")
                  (H.ConE $ H.mkName "()")]
     body = H.LetE [H.ValD pat (H.NormalB (H.VarE (qnameCG n))) []] bind
     clause = H.Clause [] (H.NormalB body) []
  return [H.FunD (H.mkName "main__") [clause]]

