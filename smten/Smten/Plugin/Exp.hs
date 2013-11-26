
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
  ty' <- topTypeCG $ varType var
  return [S.Val nm (Just ty') body']

expCG :: CoreExpr -> CG S.Exp
expCG (Var x)
  | isConstructor x = S.VarE <$> (qconnmCG $ varName x)
  | otherwise = S.VarE <$> (qnameCG $ varName x)
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

expCG (Case x v ty ms) = do
  let (ndefs, def) = findDefault ms
  vnm <- qnameCG $ varName v
  arg <- expCG x

  let argty = varType v
      mkBody | [] <- ms = mkEmptyCase vnm ty
             | isBoolType argty = mkBoolCase vnm def ndefs
             | isConcreteType argty = mkConcreteCase vnm def ndefs
             | isSmtenPrimType argty = mkSmtenPrimCase vnm argty def ndefs
             | otherwise = mkDataCase vnm argty def ndefs

  body <- mkBody
  let binds = [S.Val vnm Nothing arg]
  return $ S.LetE binds body
  
        
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
  coerce <- usequalified "GHC.Prim" "unsafeCoerce#"
  return (S.SigE (S.AppE (S.VarE coerce) (S.SigE x' at')) bt')
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


-- Empty case expressions:
--   case x of {}
-- Translated to:
--   x `seq` (emptycase :: t)
mkEmptyCase :: S.Name -> Type -> CG S.Exp
mkEmptyCase x ty = do
  ty' <- typeCG ty
  seqnm <- usequalified "Prelude" "seq"
  err <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "emptycase"
  let terr = S.SigE err ty'
  return $ S.AppE (S.AppE (S.VarE seqnm) (S.VarE x)) terr


-- mkBoolCase v mdef ms
-- Generates code for a case expression with boolean arguments:
--  ite0 v tb fb
--    where tb is the True body
--          fb is the False body
mkBoolCase :: S.Name -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkBoolCase vnm mdef ms = do
  alts <- mapM altCG ms
  let mkite tb fb = do
        itenm <- usequalified "Smten.Runtime.SmtenHS" "ite"
        return $ foldl1 S.AppE [S.VarE itenm, S.VarE vnm, tb, fb]

      getFalse :: S.Alt -> Maybe S.Exp
      getFalse (S.Alt (S.ConP n []) x)
           | take 5 (reverse n) == reverse "False" = Just x
      getFalse _ = Nothing

      getTrue :: S.Alt -> Maybe S.Exp
      getTrue (S.Alt (S.ConP n []) x)
           | take 4 (reverse n) == reverse "True" = Just x
      getTrue _ = Nothing

  case (alts, mdef) of
    ([a, b], Nothing)
       | Just fb <- getFalse a
       , Just tb <- getTrue b -> mkite tb fb
    ([a], Just b)
       | Just tb <- getTrue a -> do
            fb <- expCG b
            mkite tb fb
       | Just fb <- getFalse a -> do
            tb <- expCG b
            mkite tb fb
    ([], Just b) -> expCG b

-- mkConcreteCase vnm mdef ms
--  Generate code for a concrete case expression.
mkConcreteCase :: S.Name -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkConcreteCase vnm mdef ms = do
  ms' <- mapM altCG ms 
  def <- mkDefault mdef
  return $ S.CaseE (S.VarE vnm) (ms' ++ def)

mkDefault :: Maybe CoreExpr -> CG [S.Alt]
mkDefault Nothing = return []
mkDefault (Just b) = do
  x <- S.Alt S.wildP <$> expCG b
  return [x]

-- For "Smten" primitive types: Char and Int
--   let casef __vnm =
--         case __vnm of
--              X# ... -> ...
--              X# ... -> ...
--              X# -> default
--              Ite p a b -> ite0 p (casef a) (casef b)
--   in casef vnm
mkSmtenPrimCase :: S.Name -> Type -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkSmtenPrimCase vnm argty mdef ms = do
  alts <- mapM altCG ms
  uniqf <- lift $ getUniqueM
  uniqv <- lift $ getUniqueM
  let occf = mkVarOcc "casef"
      casef = mkSystemName uniqf occf

      occv = mkVarOcc vnm
      vnmv = mkSystemName uniqv occv

      tycon = fst (fromMaybe (error "mkSymSmtenPrim splitTyConApp failed") $
                       splitTyConApp_maybe argty)
      tynm = tyConName tycon
  
  ite0nm <- usequalified "Smten.Runtime.SmtenHS" "ite0"

  vnmv' <- qnameCG vnmv
  casefnm <- qnameCG casef
  qitenm <- qitenmCG tynm
  defalt <- mkDefault mdef
  let itebody = foldl1 S.AppE [
         S.VarE ite0nm,
         S.VarE "p",
         S.AppE (S.VarE casefnm) (S.VarE "a"),
         S.AppE (S.VarE casefnm) (S.VarE "b")]
      itepat = S.ConP qitenm [S.VarP "p", S.VarP "a", S.VarP "b"]
      itealt = S.Alt itepat itebody

      allalts = alts ++ [itealt] ++ defalt
      casee = S.CaseE (S.VarE vnmv') allalts
      lame = S.LamE vnmv' casee
      bind = S.Val casefnm Nothing lame
      ine = S.AppE (S.VarE casefnm) (S.VarE vnm)
  return (S.LetE [bind] ine)

-- For regular algebraic data types.
-- merge [(gdA v, let x1 = flA1 v
--                    x2 = flA2 v
--                    ...
--                in bodyA),
--        (gdB v, let ... in bodyB),
--        (True, default)]
mkDataCase :: S.Name -> Type -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkDataCase vnm argty mdef ms = do
  case splitTyConApp_maybe argty of
      Just (tycon, _) -> do
          let mkalt :: CoreAlt -> CG S.Exp
              mkalt (DataAlt k, xs, body) = do
                gdnm <- qguardnmCG (dataConName k)
                let first = S.AppE (S.VarE gdnm) (S.VarE vnm)
                    mkbind (i, x) = do
                      flnm <- qfieldnmCG i (dataConName k)
                      x' <- qnameCG $ varName x
                      return $ S.Val x' Nothing (S.AppE (S.VarE flnm) (S.VarE vnm)) 
                binds <- mapM mkbind (zip [1..] xs)
                body' <- expCG body
                let second = S.LetE binds body'
                return $ S.tup2E first second

          def <- case mdef of
                   Nothing -> return []
                   Just b -> do
                     tt <- S.VarE <$> usequalified "Smten.Runtime.Formula" "True"
                     e <- S.tup2E tt <$> expCG b
                     return [e]

          alts <- mapM mkalt ms
          let choices = alts ++ def
          merge <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "merge"
          return $ S.AppE merge (S.ListE choices)
      _ -> do
          lift $ errorMsg (text "SMTEN PLUGIN ERROR: no tycon for: " <+> ppr argty)
          return (S.VarE "???")
      

-- | Translate a case alternative directly to haskell,
-- without special translation for handling symbolic things.
altCG :: CoreAlt -> CG S.Alt
altCG (LitAlt l, _, body) = S.Alt (S.LitP (litCG l)) <$> expCG body
altCG (DataAlt k, xs, body) = do
  xs' <- mapM (qnameCG . varName) xs
  body' <- expCG body
  k' <- qnameCG $ getName k
  let alta = S.Alt (S.ConP k' (map S.VarP xs')) body'
  return alta

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

isConstructor :: Var -> Bool
isConstructor v = case idDetails v of
                    DataConWorkId _ -> True
                    _ -> False
            
litCG :: Literal -> S.Literal
litCG (MachStr str) = S.StringHashL (unpackFS str)
litCG (MachChar c) = S.CharL c
litCG (MachInt i) = S.IntL i
litCG (MachWord i) = S.WordL i
litCG (LitInteger i _) = S.IntegerL i
litCG MachNullAddr = S.NullAddrL
litCG (MachInt64 {}) = error "Smten Plugin TODO: litCG MachInt64"
litCG (MachWord64 {}) = error "Smten Plugin TODO: litCG MachWord64"
litCG (MachFloat x) = S.FloatL x
litCG (MachDouble x) = S.DoubleL x
litCG (MachLabel {}) = error "Smten Plugin TODO: litCG MachLabel"

