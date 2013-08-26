
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

expCG (Case x v ty ms) = caseCG x v ty ms
        
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

-- Generate code for a case expression.
caseCG :: CoreExpr -> Var -> Type -> [CoreAlt] -> CG S.Exp

-- Empty case expressions:
--   case x of {}
-- Translated to:
--   x `seq` (emptycase :: t)
caseCG x v ty [] = do
  x' <- expCG x
  ty' <- typeCG ty
  addimport "Prelude"
  addimport "Smten.Runtime.SmtenHS"
  let err = S.VarE "Smten.Runtime.SmtenHS.emptycase"
      terr = S.SigE err ty'
      seq = S.AppE (S.AppE (S.VarE "Prelude.seq") x') terr
  return seq

caseCG x v ty ms = do
  let (ndefs, def) = findDefault ms
  vnm <- qnameCG $ varName v
  arg <- expCG x

  -- generate the concrete alternatives. We always do this in order to keep
  -- concrete evaluation fast.
  (mbs, cms) <- unzip <$> mapM altCG ndefs

  -- generate the symbolic (and default) alternatives.
  sms <- mkSymAlts vnm (varType v) ty def cms
  let casee = S.CaseE (S.VarE vnm) (concat [cms, sms])
      binds = [S.Val vnm Nothing arg] ++ mbs
  return $ S.LetE binds casee
               

-- mkSymAlts vnm argty ty default alts
--  Generate the set of alternatives for the case branch which will follow the
--  concrete alternatives. This should also generate a concrete default
--  alternative if needed.
mkSymAlts :: S.Name -> Type -> Type -> Maybe CoreExpr -> [S.Alt] -> CG [S.Alt]
mkSymAlts vnm argty ty mdef ms 
 | isBoolType argty = mkSymBool vnm mdef ms
 | isConcreteType argty = mkDefault mdef
 | isSmtenPrimType argty = mkSymSmtenPrim vnm argty mdef ms
 | otherwise = mkSymData vnm argty mdef ms

-- mkSymBool v mdef ms
--   Generate the symbolic alternatives for case expression with boolean
--   argument.
--
--  _ -> ite0 v tb fb
--    where tb is the True body
--          fb is the False body
mkSymBool :: S.Name -> Maybe CoreExpr -> [S.Alt] -> CG [S.Alt]
mkSymBool vnm mdef ms = do
  let mkite tb fb = do
        addimport "Smten.Runtime.SmtenHS"
        return $ foldl1 S.AppE [
          S.VarE "Smten.Runtime.SmtenHS.ite", 
          S.VarE vnm,
          tb, fb]

      getFalse :: S.Alt -> Maybe S.Exp
      getFalse (S.Alt (S.ConP n []) x)
           | take 5 (reverse n) == reverse "False" = Just x
      getFalse _ = Nothing

      getTrue :: S.Alt -> Maybe S.Exp
      getTrue (S.Alt (S.ConP n []) x)
           | take 4 (reverse n) == reverse "True" = Just x
            
      getTrue _ = Nothing

  body <- case (ms, mdef) of
            ([a, b], Nothing)
               | Just fb <- getFalse a, Just tb <- getTrue b -> mkite tb fb
            ([a], Just b)
               | Just tb <- getTrue a -> do
                    fb <- expCG b
                    mkite tb fb
               | Just fb <- getFalse a -> do
                    tb <- expCG b
                    mkite tb fb
            ([], Just b) -> expCG b
  return [S.Alt (S.VarP "_") body]

-- mkDefault mdef
--  Generate the default alternative.
mkDefault :: Maybe CoreExpr -> CG [S.Alt]
mkDefault Nothing = return []
mkDefault (Just b) = do
  b' <- expCG b
  return [S.Alt (S.VarP "_") b']

-- For "Smten" primitive types: Char and Int
--   X# _ -> default    
--   _ -> let casef __vnm =
--              case __vnm of
--                   X# ... -> ...
--                   X# ... -> ...
--                   X# -> default
--                   Ite p a b -> ite0 p (casef a) (casef b)
--                   Prim r c -> primsapp casef r c
--                   Error msg -> error0 msg
--        in casef vnm
-- TODO: actually generate the first 'default' alternative.
mkSymSmtenPrim :: S.Name -> Type -> Maybe CoreExpr -> [S.Alt] -> CG [S.Alt]
mkSymSmtenPrim vnm argty mdef ms = do
  uniqf <- lift $ getUniqueM
  uniqv <- lift $ getUniqueM
  let occf = mkVarOcc "casef"
      casef = mkSystemName uniqf occf

      occv = mkVarOcc vnm
      vnmv = mkSystemName uniqv occv

      tycon = fst (fromMaybe (error "mkSymSmtenPrim splitTyConApp failed") $
                       splitTyConApp_maybe argty)
      tynm = tyConName tycon
  addimport "Smten.Runtime.SmtenHS"

  vnmv' <- qnameCG vnmv
  casefnm <- qnameCG casef
  qerrnm <- qerrnmCG tynm
  qitenm <- qitenmCG tynm
  qprimnm <- qprimnmCG tynm
  defalt <- mkDefault mdef
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

      allalts = ms ++ [itealt, erralt, primalt] ++ defalt
      casee = S.CaseE (S.VarE vnmv') allalts
      lame = S.LamE vnmv' casee
      bind = S.Val casefnm Nothing lame
      ine = S.AppE (S.VarE casefnm) (S.VarE vnm)
  return [S.Alt (S.VarP "_") (S.LetE [bind] ine)]

-- For regular algebraic data types.
--   _ -> let casef __vnm =
--              case __vnm of
--                FooA a b ... -> ...
--                FooB a b ... -> ...
--                ...
--                Foo_Error msg -> error0 msg
--                Foo_Ite {} -> flsapp casef __vnm [__iteFooA __vnm, __iteFooB __vnm, ...]
--                Foo_Prim {} -> ???
--                _ -> default
--        in casef vnm
mkSymData :: S.Name -> Type -> Maybe CoreExpr -> [S.Alt] -> CG [S.Alt]
mkSymData vnm argty mdef ms = do
  uniqf <- lift $ getUniqueM
  uniqv <- lift $ getUniqueM
  let occf = mkVarOcc "casef"
      casef = mkSystemName uniqf occf

      occv = mkVarOcc vnm
      vnmv = mkSystemName uniqv occv
  case splitTyConApp_maybe argty of
      Just (tycon, _) -> do
          let tynm = tyConName tycon
          addimport "Smten.Runtime.SmtenHS"

          vnmv' <- qnameCG vnmv
          casefnm <- qnameCG casef
          qerrnm <- qerrnmCG tynm
          qitenm <- qitenmCG tynm
          qiteflnms <- mapM (qiteflnmCG . dataConName) (tyConDataCons tycon)
          qiteerrnm <- qiteerrnmCG tynm
          qprimnm <- qprimnmCG tynm
          defalt <- mkDefault mdef
          let itefls = [S.AppE (S.VarE qiteflnm) (S.VarE vnmv') | qiteflnm <- qiteflnms]
              iteerr = S.AppE (S.VarE qiteerrnm) (S.VarE vnmv')
              itebody = foldl1 S.AppE [
                 S.VarE "Smten.Runtime.SmtenHS.flsapp",
                 S.VarE casefnm,
                 S.VarE vnmv',
                 S.ListE (itefls ++ [iteerr])]
              itealt = S.Alt (S.RecP qitenm) itebody

              primbody = foldl1 S.AppE (map S.VarE [
                            "Smten.Runtime.SmtenHS.primsapp", casefnm, "r", "c"])
              primalt = S.Alt (S.ConP qprimnm [S.VarP "r", S.VarP "c"]) primbody
                              
              erralt = S.Alt (S.ConP qerrnm [S.VarP "msg"])
                             (S.AppE (S.VarE "Smten.Runtime.SmtenHS.error0") (S.VarE "msg"))

              allalts = ms ++ [itealt, erralt, primalt] ++ defalt
              casee = S.CaseE (S.VarE vnmv') allalts
              lame = S.LamE vnmv' casee
              bind = S.Val casefnm Nothing lame
              ine = S.AppE (S.VarE casefnm) (S.VarE vnm)
          return [S.Alt (S.VarP "_") (S.LetE [bind] ine)]
      _ -> do
          lift $ errorMsg (text "SMTEN PLUGIN ERROR: no tycon for: " <+> ppr argty)
          return [S.Alt (S.VarP "_") (S.VarE "???")]
      

-- | Translate a case alternative directly to haskell
altCG :: CoreAlt -> CG (S.Val, S.Alt)
altCG (LitAlt l, _, body) = do
  uniqf <- lift $ getUniqueM
  let occf = mkVarOcc "f"
      ff = mkSystemName uniqf occf
  ff' <- nameCG ff
  body' <- expCG body
  let altf = S.Val ff' Nothing body'
      alta = S.Alt (S.LitP (litCG l)) (S.VarE ff')
  return (altf, alta)
altCG (DataAlt k, xs, body) = do
  uniqf <- lift $ getUniqueM
  let occf = mkVarOcc "f"
      ff = mkSystemName uniqf occf
  ff' <- nameCG ff
  xs' <- mapM (qnameCG . varName) xs
  body' <- expCG body
  k' <- qnameCG $ getName k
  ty <- topTypeCG $ mkFunTys (map varType xs) (exprType body)
  let lam = foldr (S.LamE) body' xs'
      altf = S.Val ff' (Just ty) lam

      applied = foldl S.AppE (S.VarE ff') (map S.VarE xs')
      alta = S.Alt (S.ConP k' (map S.VarP xs')) applied
  return (altf, alta)

isBoolType :: Type -> Bool
isBoolType = isType "Bool" ["Smten.Data.Bool0", "GHC.Types"]

isConcreteType :: Type -> Bool
isConcreteType t = isType "Char#" ["GHC.Prim"] t
                || isType "Int#" ["GHC.Prim"] t
                || isDictTy t

isSmtenPrimType :: Type -> Bool
isSmtenPrimType t = isType "Int" ["GHC.Types"] t
                 || isType "Char" ["GHC.Types"] t

isType :: String -> [String] -> Type -> Bool
isType wnm wmods t = 
   case splitTyConApp_maybe t of
        Just (tycon, []) -> 
            let nm = tyConName tycon
                occnm = occNameString $ nameOccName nm
                modnm = moduleNameString . moduleName <$> nameModule_maybe nm
            in occnm == wnm && modnm `elem` (map Just wmods)
        _ -> False
            
litCG :: Literal -> S.Literal
litCG (MachStr str) = S.StringL (unpackFS str)
litCG (MachChar c) = S.CharL c
litCG (MachInt i) = S.IntL i
litCG (MachWord i) = S.WordL i
litCG (LitInteger i _) = S.IntegerL i
