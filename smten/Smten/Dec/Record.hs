
module Smten.Dec.Record (
    ConRec(..), recordD, recordC, recordU,
    derive,
    ) where

import Data.List(nub)

import Smten.Name
import Smten.Location
import Smten.Type
import Smten.Sig
import Smten.Exp
import Smten.Dec.Dec

-- | Record type constructors.
data ConRec = NormalC Name [Type]
            | RecordC Name [(Name, Type)]
    deriving(Eq, Show)


-- return the undef variable name for a given data constructor name.
record_undefnm :: Name -> Name
record_undefnm n = name $ "__" ++ unname n ++ "_undef"

-- return the updater for a given field.
record_updnm :: Name -> Name
record_updnm n = name $ "__" ++ unname n ++ "_update"

-- | Desugar record constructors from a data declaration.
-- Also handles deriving construts.
-- Generates:
--   data declaration with normal constructors.
--   accessor functions for every field.
--   update functions for every field.
--   An undef declaration for each constructor.
recordD :: Location -> Name -> [TyVar] -> [ConRec] -> [Name] -> [Dec]
recordD l nm vars cons derivings =
  let mkcon :: ConRec -> Con
      mkcon (NormalC n ts) = Con n ts
      mkcon (RecordC n ts) = Con n (map snd ts)

      dt = appsT (conT nm) (map tyVarType vars)

      mkundef :: Con -> Dec
      mkundef (Con n ts) =
        let undefnm = (record_undefnm n)
            undefet = arrowsT $ ts ++ [dt]
            undefe = appsE l (ConE l (Sig n undefet)) [AppE l (VarE l (Sig (name "error") (arrowT stringT t))) (stringE l ("undefined " ++ unname n)) | t <- ts]
        in ValD l (TopExp (TopSig undefnm [] dt) undefe)

      -- TODO: handle correctly the case where two different constructors
      -- share the same accessor name.
      mkaccs :: ConRec -> [Dec]
      mkaccs (NormalC {}) = []
      mkaccs (RecordC cn ts) = 
        let mkacc :: ((Name, Type), Int) -> Dec
            mkacc ((n, t), i) = 
              let at = arrowsT [dt, t] 
                  pat = ConP cn ([WildP | _ <- take i ts]
                         ++ [VarP (name "x")]
                         ++ [WildP | _ <- drop (i+1) ts])
                  body = mlamE l [pat] (varE l (Sig (name "x") t))
              in ValD l (TopExp (TopSig n [] at) body)
        in map mkacc (zip ts [0..])

      mkupds :: ConRec -> [Dec]
      mkupds (NormalC {}) = []
      mkupds (RecordC cn ts) = 
        let ct = arrowsT $ (map snd ts) ++ [dt]
            mkupd :: ((Name, Type), Int) -> Dec
            mkupd ((n, t), i) =
              let ut = arrowsT [t, dt, dt]
                  mypat = ConP cn (
                            [VarP nm | (nm, _) <- take i ts]
                            ++ [WildP]
                            ++ [VarP nm | (nm, _) <- drop (i+1) ts])
                  myexp = appsE l (ConE l (Sig cn ct)) [VarE l (Sig n t) | (n, t) <- ts]
                  body = mlamE l [VarP n, mypat] myexp
              in ValD l (TopExp (TopSig (record_updnm n) [] ut) body)
        in map mkupd (zip ts [0..])
                            
      cons' = map mkcon cons
      undefs = map mkundef cons'
      accs = concatMap mkaccs cons
      upds = concatMap mkupds cons
      derivations = [iderive l d nm vars cons | d <- derivings]
  in concat [[DataD l nm vars cons'], derivations, undefs, accs, upds]

-- | Desugar labelled update.
recordU :: Location -> Exp -> [(Name, Exp)] -> Exp
recordU _ e [] = e
recordU l e ((n, v):us) = 
  appsE l (VarE l (Sig (record_updnm n) (arrowsT [typeof v, typeof e])))
         [v, recordU l e us]

-- | Desugar labelled constructors.
recordC :: Location -> Sig -> [(Name, Exp)] -> Exp
recordC l (Sig cn ct) fields = recordU l (VarE l (Sig (record_undefnm cn) ct)) fields

-- Derive an instance of Eq for the given data type declaration.
-- Generates something of the form:
--    instance ctx => Eq (Foo ...) where
--       (==) (Foo1 a1 a2 ...) (Foo1 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) (Foo2 a1 a2 ...) (Foo2 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--            ...
--       (==) (FooN a1 a2 ...) (FooN b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) _ _ = False
deriveEq :: Location -> Context -> Class -> [ConRec] -> Dec
deriveEq l ctx cls cs =
  let mkcon :: ConRec -> MAlt
      mkcon (NormalC cn ts) =
        let fieldsA = [Sig (name $ 'a' : show i) UnknownT | i <- [1..length ts]]
            fieldsB = [Sig (name $ 'b' : show i) UnknownT | i <- [1..length ts]]
            pA = ConP cn [VarP n | Sig n _ <- fieldsA]
            pB = ConP cn [VarP n | Sig n _ <- fieldsB]
            body = appsE l (VarE l (Sig (name "and") UnknownT))
                    [listE l [appsE l (VarE l (Sig (name "==") UnknownT)) 
                               [VarE l a, VarE l b] | (a, b) <- zip fieldsA fieldsB]]
        in simpleMA l [pA, pB] body []
      mkcon (RecordC cn ts) = mkcon (NormalC cn (map snd ts))

      def = simpleMA l [WildP, WildP] (falseE l) []
      eqclauses = map mkcon cs ++ [def]
      eq = Method (name "==") (clauseE l eqclauses)
  in InstD l ctx cls [eq]

-- Derive an instance of Ord for the given data type declaration.
-- Generates something of the form:
--    instance ctx => Ord (Foo ...) where
--       (<=) (Foo1 a1 a2 ...) (Foo1 b1 b2 ...) =
--              a1 < b1 || (a1 == b1 && a2 < b2 || ...)
--       (<=) (Foo1 a1 a2 ...) (Foo2 b1 b2 ...) = False
--       (<=) (Foo1 a1 a2 ...) (Foo3 b1 b2 ...) = False
--            ...
--       (<=) (Foo2 a1 a2 ...) (Foo1 b1 b2 ...) = True
--       (<=) (Foo2 a1 a2 ...) (Foo2 b1 b2 ...) =
--              a1 < b1 || (a1 == b1 && a2 < b2 || ...)
--            ...
deriveOrd :: Location -> Context -> Class -> [ConRec] -> Dec
deriveOrd l ctx cls cs =
  let mkclause :: (Name, [Type]) -> (Name, [Type]) -> Exp -> MAlt
      mkclause (ln, lts) (rn, rts) body =
        let pl = ConP ln [VarP (name $ "a" ++ show i) | i <- [1..length lts]]
            pr = ConP rn [VarP (name $ "b" ++ show i) | i <- [1..length rts]]
        in simpleMA l [pl, pr] body []

      mkop :: String -> Int -> Exp
      mkop op n = appsE l (varE l (Sig (name op) UnknownT)) [
                           varE l (Sig (name $ "a" ++ show n) UnknownT),
                           varE l (Sig (name $ "b" ++ show n) UnknownT)]

      mkcmp :: Int -> Int -> Exp
      mkcmp lo hi 
        | lo < hi = 
            let lt = mkop "Prelude.<" lo
                eq = mkop "Prelude.==" lo
                rest = mkcmp (lo+1) hi
                eqcase = appsE l (varE l (Sig (name "Prelude.&&") UnknownT)) [eq, rest]
            in appsE l (varE l (Sig (name "Prelude.||") UnknownT)) [lt, eqcase]
        | lo == hi = mkop "Prelude.<=" lo
        | lo > hi = trueE l

      mkleft :: (ConRec, Integer) -> [MAlt]
      mkleft (NormalC ln lts, li) =
          let mkright :: (ConRec, Integer) -> MAlt
              mkright (NormalC rn rts, ri)
                | li < ri = mkclause (ln, lts) (rn, rts) (trueE l)
                | li == ri = mkclause (ln, lts) (rn, rts) (mkcmp 1 (length lts))
                | li > ri = mkclause (ln, lts) (rn, rts) (falseE l)
              mkright (RecordC rn rts, ri) = mkright (NormalC rn (map snd rts), ri)
          in map mkright (zip cs [0..])
      mkleft (RecordC ln lts, li) = mkleft (NormalC ln (map snd lts), li)

      clauses = concatMap mkleft (zip cs [0..])
      ord = Method (name "<=") (clauseE l clauses)
      inst = InstD l ctx cls [ord]
  in inst

-- Derive an instance of Free (before qualify and inference) for the given
-- data type declaration.
--
-- TODO: this should only work for non-recursive data types!
--
-- For example:
--   data Foo = Bar Integer Integer
--            | Sludge Bool
--            | Wedge
--
-- Derives something of the form:
-- Generates something of the form
-- instance ctx => Free (Foo ...) where
--   free = do
--      isFoo1 <- free ; isFoo2 <- free ; ... ; isFooNm1 <- free 
--
--      a1Foo1 <- free ; a2Foo1 <- free ; ... 
--      a1Foo2 <- free ; a2Foo2 <- free ; ... 
--      ...
--      a1FooN <- free ; a2FooN <- free ; ... 
--
--      return (
--               if isFoo1 then Foo1 a1Foo1 a2Foo1 ...
--          else if isFoo2 then Foo2 a1Foo2 a2Foo2 ...
--             ...
--          else FooN a1FooN a2FooN ...)
deriveFree :: Location -> Context -> Class -> [ConRec] -> Dec
deriveFree l ctx cls cs =
  let -- name of tag for constructor: (isFooX :: Bool)
      mkTag :: ConRec -> Sig
      mkTag (NormalC nm _) = Sig (name $ "is" ++ unname nm) boolT
      mkTag (RecordC nm ts) = mkTag (NormalC nm (map snd ts))

      -- fields for constructor: [a1FooX, a2FooX, ...]
      mkFields :: ConRec -> [Sig]
      mkFields (NormalC nm ts)
        = [Sig (name $ ('a' : show i) ++ unname nm) UnknownT | i <- [1..length ts]]
      mkFields (RecordC nm ts) = mkFields (NormalC nm (map snd ts))

      -- application of constructor to its fields:
      --    FooX a1FooX a2FooX ...
      mkCon :: ConRec -> Exp
      mkCon c@(NormalC nm _) =
        let fields = mkFields c
        in appsE l (conE l (Sig nm UnknownT)) (map (varE l) fields)
      mkCon (RecordC nm ts) = mkCon (NormalC nm (map snd ts))

      -- given [t1, t2, ..., tnm1] [v1, v2, ... vn]
      --  Generates the nested if statement:
      --    if t1 then v1
      --          else if t2 then v2
      --                     else if ...
      --                             else vn
      mkIf :: [Sig] -> [Exp] -> Exp
      mkIf [] [x] = x
      mkIf (t:ts) (x:xs) = ifE l (varE l t) x (mkIf ts xs)
      mkIf ss es = error $ "mkIf: " ++ show ss ++ ", " ++ show es

      tags = map mkTag (init cs)
      fields = concat (map mkFields cs)
      freevars = [BindS (VarP n) (VarE l (Sig (name "free") UnknownT))
                    | Sig n _ <- tags ++ fields]
      bodies = map mkCon cs
      value = mkIf tags bodies
      rtn = NoBindS $ appE l (varE l (Sig (name "return") UnknownT)) value
      stmts = freevars ++ [rtn]
      free = Method (name "free") (doE l stmts)
  in InstD l ctx cls [free]

-- Derive an instance of Show for the given data type declaration.
-- Generates something of the form:
--    instance ctx => Show (Foo ...) where
--       show (Foo1 a1 a2 ...) = show_helper ["Foo1", show a1, show a2, ...]
--       show (Foo2 a1 a2 ...) = show_helper ["Foo2", show a1, show a2, ...]
--            ...
--       show (FooN a1 a2 ...) = show_helper ["FooN", show a1, show a2, ...]
deriveShow :: Location -> Context -> Class -> [ConRec] -> Dec
deriveShow l ctx cls cs =
  let mkcon :: ConRec -> MAlt
      mkcon (NormalC cn ts) =
        let fields = [Sig (name $ 'a' : show i) UnknownT | i <- [1..length ts]]
            p = ConP cn [VarP n | Sig n _ <- fields]
            shows = [appE l (VarE l (Sig (name "show") UnknownT)) (VarE l a) | a <- fields]
            body = appE l (VarE l (Sig (name "__show_helper") UnknownT)) $
                     listE l (stringE l (unname cn) : shows)
        in simpleMA l [p] body []
      mkcon (RecordC cn ts) = mkcon (NormalC cn (map snd ts))

      shclauses = map mkcon cs
      sh = Method (name "show") (clauseE l shclauses)
  in InstD l ctx cls [sh]

derive :: Location -> Context -> Class -> [ConRec] -> Dec
derive l ctx cls@(Class n _)
 | n == name "Eq" = deriveEq l ctx cls
 | n == name "Ord" = deriveOrd l ctx cls
 | n == name "Free" = deriveFree l ctx cls
 | n == name "Show" = deriveShow l ctx cls
 | otherwise = error $ "deriving " ++ show n ++ " not supported in smten"
      
iderive :: Location -> Name -> Name -> [TyVar] -> [ConRec] -> Dec
iderive l n dn vars cs = 
  let dt = appsT (conT dn) (map tyVarType vars)
      cls = Class n [dt]
    
      keep :: Type -> Bool
      keep t = not $ or [null (varTs t), isSubType dt t]

      fields :: ConRec -> [Type]
      fields (NormalC _ ts) = ts
      fields (RecordC _ ts) = map snd ts

      fieldts = concatMap fields cs
      ctx = nub [Class n [t] | t <- filter keep fieldts]
  in derive l ctx cls cs

