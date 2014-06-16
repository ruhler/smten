
{-# LANGUAGE PatternGuards #-}

module Smten.Plugin.Exp (
    bindCG
  ) where

import Data.Functor

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
  ty' <- typeCG $ varType var
  return [S.Val nm (Just ty') body']

expCG :: CoreExpr -> CG S.Exp
expCG (Var x)
  | isConstructor x = S.VarE <$> (qconnmCG $ varName x)
  | otherwise = S.VarE <$> (qnameCG $ varName x)
expCG (Lit l@(MachInt {})) = do
    mkint <- usequalified "Smten.Runtime.Int" "int#"
    return $ S.AppE (S.VarE mkint) (S.LitE (litCG l))
expCG (Lit l@(MachChar {})) = do
    mkchar <- usequalified "Smten.Runtime.Char" "char#"
    return $ S.AppE (S.VarE mkchar) (S.LitE (litCG l))
expCG (Lit l) = return (S.LitE (litCG l))
expCG x@(App a t@(Type {})) = do
  let tya = exprType a
      tyb = applyTypeToArg tya t
  tyb' <- typeCG tyb
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
    ty <- typeCG $ exprType x
    body' <- expCG body
    return $ S.SigE body' ty
 | otherwise = do
    b' <- qnameCG $ varName b
    body' <- expCG body
    return $ S.LamE b' body'

expCG e@(Case x v ty ms) = do
  let (ndefs, def) = findDefault ms
  vnm <- qnameCG $ varName v
  arg <- expCG x

  let argty = varType v
      mkBody | [] <- ms = mkEmptyCase vnm ty
             | isBoolType argty = mkBoolCase vnm def ndefs
             | isConcreteType argty = mkConcreteCase vnm def ndefs
             | Just (tycon, _) <- splitTyConApp_maybe argty
                 = mkDataCase vnm tycon def ndefs
             | ndefs <- [], Just defval <- def = mkSeqCase vnm defval
             | otherwise = do
                  lift $ errorMsg (text "SMTEN PLUGIN ERROR: TODO: translate " <+> ppr e)
                  return (S.VarE "???")

  body <- mkBody
  let binds = [S.Val vnm Nothing arg]
  return $ S.LetE binds body

expCG (Cast x c) = castCG True x c

expCG (Tick (ProfNote cc _ _) x) = do
   x' <- expCG x
   return (S.SccE (unpackFS $ cc_name cc) x')

expCG x = do
  lift $ fatalErrorMsg (text "TODO: expCG " <+> ppr x)
  return (S.VarE "???")

-- castCG p x c
--   p - the polarity of the cast.
--     True means: go from fst of coercionKind to snd of coercionKind
--     False means: go from snd of coercionKind to fst of coercionKind
castCG :: Bool -> CoreExpr -> Coercion -> CG S.Exp
castCG p x c =
  case c of
    AxiomInstCo {} -> do
      let lhs = dropForAlls . fst . unPair $ coercionKind c
      case splitTyConApp_maybe lhs of
         Just (tycon, _) | isNewTyCon tycon ->
            case p of
              True -> do
                -- This is a de-newtype cast: from NT Foo to Foo
                let [dc] = tyConDataCons tycon
                denew <- S.VarE <$> qdenewtynmCG (dataConName dc)
                x' <- expCG x
                return $ S.AppE denew x'
              False -> do
                -- This is a newtype cast: from Foo to NT Foo
                let [dc] = tyConDataCons tycon
                new <- S.VarE <$> qnameCG (dataConName dc)
                x' <- expCG x
                return $ S.AppE new x'
         _ -> unknownCastCG p x c
    ForAllCo _ c' -> do
      let ty = (if p then snd else fst) (unPair $ coercionKind c)
      ty' <- typeCG ty
      x' <- castCG p x c'
      return $ S.SigE x' ty'
    TransCo c1 c2 ->
      case p of
        True -> castCG True (Cast x c1) c2
        False -> castCG False (Cast x (SymCo c2)) c1
    SymCo c' -> castCG (not p) x c'
    _ -> unknownCastCG p x c

-- TODO: Ideally we should be able to handle every cast, and shouldn't
-- have to rely on this.
unknownCastCG :: Bool -> CoreExpr -> Coercion -> CG S.Exp
unknownCastCG p x c = do
  lift $ debugTraceMsg (text "unknownCastCG: " <+> ppr (Cast x c))
  let (at, bt) = unPair $ coercionKind c
      (st, dt) = if p then (at, bt) else (bt, at)
  x' <- expCG x
  st' <- typeCG st
  dt' <- typeCG dt
  coerce <- usequalified "GHC.Prim" "unsafeCoerce#"
  return (S.SigE (S.AppE (S.VarE coerce) (S.SigE x' st')) dt')
   

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

-- Case expression with single default branch.
-- Note: the argument may be of any type, not necessarally data type.
--  case x of { _ -> v }
-- Translated to:
--  x `seq` v
mkSeqCase :: S.Name -> CoreExpr -> CG S.Exp
mkSeqCase x v = do
  seqnm <- usequalified "Prelude" "seq"
  v' <- expCG v
  return $ S.AppE (S.AppE (S.VarE seqnm) (S.VarE x)) v'

mkIte :: S.Exp -> S.Exp -> S.Exp -> CG S.Exp
mkIte p a b = do
  itenm <- usequalified "Smten.Runtime.SmtenHS" "ite0"
  return $ foldl1 S.AppE [S.VarE itenm, p, a, b]

-- mkBoolCase v mdef ms
-- Generates code for a case expression with boolean arguments:
--  ite0 v tb fb
--    where tb is the True body
--          fb is the False body
mkBoolCase :: S.Name -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkBoolCase vnm mdef ms = do
  alts <- mapM altCG ms
  let mkite = mkIte (S.VarE vnm)

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

-- For regular algebraic data types.
-- case v of
--   Foo { gdA = gdA, flA1 = a1, flA2 = a2, ...,
--         gdB = gdB, flB1 = b1, flB2 = b2, ...,
--         ...
--         } -> ite0 gda bodyA (ite gdB bodyB ( ... default)
-- Note: this works only under the assumption that the variable names for the
-- fields in different alternatives are all unique. I think that's a safe
-- assumption given the uniqification of names ghc does before going into
-- core.
mkDataCase :: S.Name -> TyCon -> Maybe CoreExpr -> [CoreAlt] -> CG S.Exp
mkDataCase vnm tycon mdef ms = do
   let -- mkalt returns: (gd, val, fields)
       --   where gd is the guard for the alternative
       --         val is the body of the alternative
       --         fields are the fields required for the alternative 
       mkalt :: CoreAlt -> CG (S.Exp, S.Exp, [S.PatField])
       mkalt (DataAlt k, xs, body) = do
         gdnm <- guardnmCG (dataConName k)
         qgdnm <- qguardnmCG (dataConName k)
         let gdfield = S.PatField qgdnm (S.VarP gdnm)
             mkbind (i, x) = do
               flnm <- qfieldnmCG i (dataConName k)
               x' <- qnameCG $ varName x
               return $ S.PatField flnm (S.VarP x')
         binds <- mapM mkbind (zip [1..] xs)
         body' <- expCG body
         return (S.VarE gdnm, body', gdfield:binds)
       mkalt (LitAlt l, [], body) = do
         islit <- S.VarE <$> qislitnmCG (tyConName tycon)
         let l' = litCG l
         body' <- expCG body
         return (S.AppE (S.AppE islit (S.LitE l')) (S.VarE vnm), body', [])

   -- TODO: The performance of concrete evaluation in Smten is very sensitive
   -- to this code, see if we can make it better?
   -- For example: if we assume the last case is reachable, we can improve
   -- concrete performance a fair amount.
   alts <- mapM mkalt ms
   let merge []
         | Just b <- mdef = expCG b
       merge [(p, x, _)]
         | Nothing <- mdef = do
             isfalse <- S.VarE <$> usequalified "Smten.Runtime.Formula" "isFalseF"
             unreach <- S.VarE <$> usequalified "Smten.Runtime.SmtenHS" "unreachable"
             true <- usequalified "Prelude" "True"
             false <- usequalified "Prelude" "False"
             return $ S.CaseE (S.AppE isfalse p) [
                        S.Alt (S.ConP true []) unreach,
                        S.Alt (S.ConP false []) x]
       merge ((p, a, _):xs) = do
          xs' <- merge xs
          mkIte p a xs'

       mergetop [(_, x, _)] | Nothing <- mdef = return x
       mergetop xs = merge xs

       fields = concat [fs | (_, _, fs) <- alts]
   body <- mergetop alts
   tynm <- qtynameCG $ tyConName tycon
   return $ S.CaseE (S.VarE vnm) [S.Alt (S.RecP tynm fields) body]

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
isConcreteType t = isType "(#,#)" ["GHC.Prim"] t || isDictTy t

isType :: String -> [String] -> Type -> Bool
isType wnm wmods t = 
   case splitTyConApp_maybe t of
        Just (tycon, _) -> 
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

