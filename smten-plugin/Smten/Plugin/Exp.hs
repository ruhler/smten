
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Exp (
    bindCG
  ) where

import Data.Functor
import Data.Maybe

import CostCentre
import Pair
import GhcPlugins

import Smten.Plugin.CG
import Smten.Plugin.Name
import Smten.Plugin.Type

import qualified Smten.Plugin.Output.Syntax as S

bindCG :: CoreBind -> CG [S.Val]
bindCG (Rec xs) = concat <$> mapM bindCG [NonRec x v | (x, v) <- xs]
bindCG b@(NonRec var body) = do
  --lift $ putMsg (ppr b)
  body' <- expCG body
  nm <- nameCG $ varName var
  if isExportedId var
     then addexport (S.VarExport nm)    
     else return ()
  ty' <- topTypeCG $ varType var
  return [S.Val nm (Just ty') body']

expCG :: CoreExpr -> CG S.Exp
expCG (Var x) = S.VarE <$> (qnameCG $ varName x)
expCG (Lit l) = return (S.LitE (litCG l))
expCG x@(App a t@(Type {})) = do
  let tya = exprType a
      tyb = applyTypeToArg tya t
  tyb' <- topTypeCG tyb
  a' <- expCG a
  return (S.SigE a' tyb')
expCG (App a b) = do
    a' <- expCG a
    b' <- expCG b
    return $ S.AppE a' b'
expCG (Let x body) = do
    x' <- bindCG x
    body' <- expCG body
    return $ S.LetE x' body'
expCG x@(Lam b body)
 | isTyVar b = do
    ty <- topTypeCG $ exprType x
    body' <- expCG body
    return $ S.SigE body' ty
 | otherwise = do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ S.LamE b' body'

-- Empty case expressions:
--  case x of {}
-- 
-- Is translated as:  x `seq` (error "inaccessable case" :: t)
expCG (Case x v ty []) = do
  x' <- expCG x
  ty' <- typeCG ty
  addimport "Prelude"
  addimport "Smten.Runtime.SmtenHS"
  let err = S.VarE "Smten.Runtime.SmtenHS.emptycase"
      terr = S.SigE err ty'
      seq = S.AppE (S.AppE (S.VarE "Prelude.seq") x') terr
  return seq

-- Boolean case expressions are generated specially as:
--   let v = x
--   in ite v t f
expCG (Case x v ty ms) | isBoolType (varType v) =
  let mkite tb fb = do
        addimport "Smten.Runtime.SmtenHS"
        vnm <- qnameCG $ varName v
        arg <- expCG x
        tb' <- expCG tb
        fb' <- expCG fb
        let ite = foldl1 S.AppE [
              S.VarE "Smten.Runtime.SmtenHS.ite", 
              S.VarE vnm,
              tb', fb']
        return $ S.LetE [S.Val vnm Nothing arg] ite
  in case ms of
       [(DataAlt fk, [], fb), (DataAlt tk, [], tb)]
          | isFalseK fk && isTrueK tk -> mkite tb fb
       [(DEFAULT, _, fb), (DataAlt tk, [], tb)] | isTrueK tk -> mkite tb fb
       [(DEFAULT, _, tb), (DataAlt fk, [], fb)] | isFalseK fk -> mkite tb fb
       [(DEFAULT, _, b)] -> do
           vnm <- qnameCG $ varName v
           arg <- expCG x
           b' <- expCG b
           return $ S.LetE [S.Val vnm Nothing arg] b'
       _ -> do
           lift $ fatalErrorMsg (text "TODO: expCG Bool Case: " <+> ppr ms)
           return $ S.VarE "???"

-- Generate normal case expressions for concrete types.
expCG (Case x v ty ms) | isConcreteType (varType v) = do
  vnm <- qnameCG $ varName v
  arg <- expCG x

  (defalt, nodefms) <- case ms of
                      ((DEFAULT, _, body) : xs) -> do
                         body' <- expCG body
                         return ([S.Alt (S.VarP vnm) body'], xs)
                      _ -> return ([], ms)

  let alt (LitAlt l, _, body) = do
        body' <- expCG body
        return $ S.Alt (S.AsP vnm (S.LitP (litCG l))) body'
      alt (DataAlt k, xs, body) = do
        body' <- expCG body
        xs' <- mapM (qnameCG .varName) xs
        k' <- qnameCG $ getName k
        return $ S.Alt (S.AsP vnm (S.ConP k' (map S.VarP xs'))) body'
  alts <- mapM alt nodefms
  return $ S.CaseE arg (alts ++ defalt)

-- Char, Int are special.
expCG (Case x v ty ms) | isCharType (varType v) || isIntType (varType v) = do
  uniq <- lift $ getUniqueM
  let occ = mkVarOcc "casef"
      casef = mkSystemName uniq occ
      tycon = fst (fromMaybe (error "Char,Int splitTyConApp failed") $
                       splitTyConApp_maybe (varType v))
      tynm = tyConName tycon
  addimport "Smten.Runtime.SmtenHS"

  vnm <- qnameCG $ varName v
  casefnm <- qnameCG casef
  qerrnm <- qerrnmCG tynm
  qitenm <- qitenmCG tynm
  qprimnm <- qprimnmCG tynm
  arg <- expCG x

  (defalt, nodefms) <- case ms of
                      ((DEFAULT, _, body) : xs) -> do
                         body' <- expCG body
                         return ([S.Alt S.wildP body'], xs)
                      _ -> return ([], ms)

  alts <- mapM altCG nodefms
  let itebody = foldl1 S.AppE [
         S.VarE "Smten.Runtime.SmtenHS.ite0",
         S.VarE "p",
         S.AppE (S.VarE casefnm) (S.VarE "a"),
         S.AppE (S.VarE casefnm) (S.VarE "b")]
      itepat = S.ConP qitenm [S.VarP "p", S.VarP "a", S.VarP "b"]
      itealt = S.Alt itepat itebody

      primbody = foldl1 S.AppE (map S.VarE [
                    "Smten.Runtime.SmtenHS.primsapp", casefnm, "r", "c"])
      primalt = S.Alt (S.ConP qprimnm [S.VarP "r", S.VarP "c"]) primbody
                      
      erralt = S.Alt (S.ConP qerrnm [S.VarP "msg"])
                     (S.AppE (S.VarE "Smten.Runtime.SmtenHS.error0") (S.VarE "msg"))

      allalts = alts ++ [itealt, erralt, primalt] ++ defalt
      casee = S.CaseE (S.VarE vnm) allalts
      lame = S.LamE vnm casee
      bind = S.Val casefnm Nothing lame
      ine = S.AppE (S.VarE casefnm) arg
  return $ S.LetE [bind] ine
        
-- General case expressions are generated as follows:
--   let casef_XX = \v ->
--          case v of
--             FooA a b ... -> ...
--             FooB a b ... -> ...
--             ...
--             Foo_Error msg -> error0 msg
--             Foo_Ite {} -> flsapp casef_XX v [__iteFooA v, __iteFooB v, ...]
--             Foo_Prim {} -> ???
--             _ -> default_body
--   in case_XX x
expCG (Case x v ty ms) = do
  uniq <- lift $ getUniqueM
  let occ = mkVarOcc "casef"
      casef = mkSystemName uniq occ
  case splitTyConApp_maybe (varType v) of
      Just (tycon, _) -> do
          let tynm = tyConName tycon
          addimport "Smten.Runtime.SmtenHS"

          vnm <- qnameCG $ varName v
          casefnm <- qnameCG casef
          qerrnm <- qerrnmCG tynm
          qitenm <- qitenmCG tynm
          qiteflnms <- mapM (qiteflnmCG . dataConName) (tyConDataCons tycon)
          qiteerrnm <- qiteerrnmCG tynm
          qprimnm <- qprimnmCG tynm
          arg <- expCG x

          (defalt, nodefms) <- case ms of
                              ((DEFAULT, _, body) : xs) -> do
                                 body' <- expCG body
                                 return ([S.Alt S.wildP body'], xs)
                              _ -> return ([], ms)

          alts <- mapM altCG nodefms
          let itefls = [S.AppE (S.VarE qiteflnm) (S.VarE vnm) | qiteflnm <- qiteflnms]
              iteerr = S.AppE (S.VarE qiteerrnm) (S.VarE vnm)
              itebody = foldl1 S.AppE [
                 S.VarE "Smten.Runtime.SmtenHS.flsapp",
                 S.VarE casefnm,
                 S.VarE vnm,
                 S.ListE (itefls ++ [iteerr])]
              itealt = S.Alt (S.RecP qitenm) itebody

              primbody = foldl1 S.AppE (map S.VarE [
                            "Smten.Runtime.SmtenHS.primsapp", casefnm, "r", "c"])
              primalt = S.Alt (S.ConP qprimnm [S.VarP "r", S.VarP "c"]) primbody
                              
              erralt = S.Alt (S.ConP qerrnm [S.VarP "msg"])
                             (S.AppE (S.VarE "Smten.Runtime.SmtenHS.error0") (S.VarE "msg"))

              allalts = alts ++ [itealt, erralt, primalt] ++ defalt
              casee = S.CaseE (S.VarE vnm) allalts
              lame = S.LamE vnm casee
              bind = S.Val casefnm Nothing lame
              ine = S.AppE (S.VarE casefnm) arg
          return $ S.LetE [bind] ine
      _ -> do
          lift $ errorMsg (text "SMTEN PLUGIN ERROR: no tycon for: " <+> ppr (varType v))
          return (S.VarE "???")

-- newtype construction cast
expCG (Cast x c)
  | Just dcnm <- newtypeCast c = do
      dcnm' <- qnameCG dcnm
      x' <- expCG x
      return $ S.AppE (S.VarE dcnm') x'

-- newtype de-construction cast
expCG (Cast x c)
  | Just dcnm <- denewtypeCast c = do
      dcnm' <- qdenewtynmCG dcnm
      x' <- expCG x
      return $ S.AppE (S.VarE dcnm') x'

expCG (Cast x c) = do
  --lift $ errorMsg (text "Warning: Using unsafeCoerce for cast " <+> ppr x <+> showco c)
  x' <- expCG x
  let (at, bt) = unPair $ coercionKind c
  at' <- topTypeCG $ dropForAlls at
  bt' <- topTypeCG bt
  addimport "GHC.Prim"
  return (S.SigE (S.AppE (S.VarE "GHC.Prim.unsafeCoerce#") (S.SigE x' at')) bt')
  --return (S.AppE (S.VarE "GHC.Prim.unsafeCoerce#") x')
expCG (Tick (ProfNote cc _ _) x) = do
   x' <- expCG x
   return (S.SccE (unpackFS $ cc_name cc) x')

expCG x = do
  lift $ fatalErrorMsg (text "TODO: expCG " <+> ppr x)
  return (S.VarE "???")

litCG :: Literal -> S.Literal
litCG (MachStr str) = S.StringL (unpackFS str)
litCG (MachChar c) = S.CharL c
litCG (MachInt i) = S.IntL i
litCG (MachWord i) = S.WordL i
litCG (LitInteger i _) = S.IntegerL i

altCG :: CoreAlt -> CG S.Alt
altCG (DataAlt k, xs, body) = do
    body' <- expCG body
    xs' <- mapM (qnameCG . varName) xs
    k' <- qnameCG $ getName k
    return $ S.Alt (S.ConP k' (map S.VarP xs')) body'
altCG (LitAlt l, _, body) = do
    body' <- expCG body
    return $ S.Alt (S.LitP (litCG l)) body'

isBoolType :: Type -> Bool
isBoolType = isType "Bool" ["Smten.Data.Bool0", "GHC.Types"]

isConcreteType :: Type -> Bool
isConcreteType t = isType "Char#" ["GHC.Prim"] t
                || isType "Int#" ["GHC.Prim"] t
                || isDictTy t

isIntType :: Type -> Bool
isIntType = isType "Int" ["GHC.Types"]

isCharType :: Type -> Bool
isCharType = isType "Char" ["GHC.Types"]

isType :: String -> [String] -> Type -> Bool
isType wnm wmods t = 
   case splitTyConApp_maybe t of
        Just (tycon, []) -> 
            let nm = tyConName tycon
                occnm = occNameString $ nameOccName nm
                modnm = moduleNameString . moduleName <$> nameModule_maybe nm
            in occnm == wnm && modnm `elem` (map Just wmods)
        _ -> False
            

isFalseK :: DataCon -> Bool
isFalseK d =
  let nm = dataConName d
  in "False" == (occNameString $ nameOccName nm)

isTrueK :: DataCon -> Bool
isTrueK d =
  let nm = dataConName d
  in "True" == (occNameString $ nameOccName nm)

-- Test if the coercion looks like a newtype cast.
-- If so, returns the name of the data constructor to use for the conversion.
newtypeCast :: Coercion -> Maybe Name
newtypeCast c = 
  let rhs = dropForAlls . snd . unPair $ coercionKind c
  in case splitTyConApp_maybe rhs of
        Just (tycon, _) | isNewTyCon tycon ->
           let [dcon] = tyConDataCons tycon
           in Just $ dataConName dcon
        _ -> Nothing
        
-- Test if the coercion looks like a de-newtype cast.
-- If so, returns the name of the data constructor to use for the de-conversion.
denewtypeCast :: Coercion -> Maybe Name
denewtypeCast c = 
  let lhs = dropForAlls . fst . unPair $ coercionKind c
  in case splitTyConApp_maybe lhs of
        Just (tycon, _) | isNewTyCon tycon ->
           let [dcon] = tyConDataCons tycon
           in Just $ dataConName dcon
        _ -> Nothing

