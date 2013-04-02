
module Smten.Dec.Record (
    ConRec(..), recordD, recordC, recordU,
    derive,
    ) where

import Data.List(nub)

import Smten.Name
import Smten.Location
import Smten.Type
import Smten.Sig
import Smten.Fresh
import Smten.Exp
import Smten.Dec.Dec

-- | Record type constructors.
data ConRec = NormalC Name [Type]
            | RecordC Name [(Name, Type)]
    deriving(Eq, Show)


-- return the undef variable name for a given data constructor name.
record_undefnm :: Name -> Name
record_undefnm n = name "__" `nappend` n `nappend` name "_undef"

-- return the updater for a given field.
record_updnm :: Name -> Name
record_updnm n = name "__" `nappend` n `nappend` name "_update"

-- | Desugar record constructors from a data declaration.
-- Also handles deriving construts.
-- Generates:
--   data declaration with normal constructors.
--   accessor functions for every field.
--   update functions for every field.
--   An undef declaration for each constructor.
recordD :: Location -> Name -> [TyVar] -> [ConRec] -> [Name] -> [Dec]
recordD loc nm vars cons derivings =
  let mkcon :: ConRec -> Con
      mkcon (NormalC n ts) = Con n ts
      mkcon (RecordC n ts) = Con n (map snd ts)

      dt = appsT (conT nm) (map tyVarType vars)

      mkundef :: Con -> Dec
      mkundef (Con n ts) =
        let undefnm = (record_undefnm n)
            undefet = arrowsT $ ts ++ [dt]
            undefe = appsE (ConE (Sig n undefet)) [VarE (Sig (name "undefined") t) | t <- ts]
        in ValD loc (TopExp (TopSig undefnm [] dt) undefe)

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
                  body = mlamE [pat] (varE (Sig (name "x") t))
              in ValD loc (TopExp (TopSig n [] at) body)
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
                  myexp = appsE (ConE (Sig cn ct)) [VarE (Sig n t) | (n, t) <- ts]
                  body = mlamE [VarP n, mypat] myexp
              in ValD loc (TopExp (TopSig (record_updnm n) [] ut) body)
        in map mkupd (zip ts [0..])
                            
      cons' = map mkcon cons
      undefs = map mkundef cons'
      accs = concatMap mkaccs cons
      upds = concatMap mkupds cons
      derivations = [iderive loc d nm vars cons | d <- derivings]
  in concat [[DataD loc nm vars cons'], derivations, undefs, accs, upds]

-- | Desugar labelled update.
recordU :: Exp -> [(Name, Exp)] -> Exp
recordU e [] = e
recordU e ((n, v):us) = 
  appsE (VarE (Sig (record_updnm n) (arrowsT [typeof v, typeof e])))
         [v, recordU e us]

-- | Desugar labelled constructors.
recordC :: Sig -> [(Name, Exp)] -> Exp
recordC (Sig cn ct) fields = recordU (VarE (Sig (record_undefnm cn) ct)) fields

-- Derive an instance of Eq for the given data type declaration.
-- Generates something of the form:
--    instance ctx => Eq (Foo ...) where
--       (==) (Foo1 a1 a2 ...) (Foo1 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) (Foo2 a1 a2 ...) (Foo2 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--            ...
--       (==) (FooN a1 a2 ...) (FooN b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) _ _ = False
deriveEq :: Location -> Context -> Class -> [ConRec] -> Dec
deriveEq loc ctx cls cs =
  let mkcon :: ConRec -> MAlt
      mkcon (NormalC cn ts) =
        let fieldsA = [Sig (name $ 'a' : show i) UnknownT | i <- [1..length ts]]
            fieldsB = [Sig (name $ 'b' : show i) UnknownT | i <- [1..length ts]]
            pA = ConP cn [VarP n | Sig n _ <- fieldsA]
            pB = ConP cn [VarP n | Sig n _ <- fieldsB]
            body = appsE (VarE (Sig (name "and") UnknownT))
                    [listE [appsE (VarE (Sig (name "==") UnknownT)) 
                               [VarE a, VarE b] | (a, b) <- zip fieldsA fieldsB]]
        in simpleMA [pA, pB] body []
      mkcon (RecordC cn ts) = mkcon (NormalC cn (map snd ts))

      def = simpleMA [WildP, WildP] falseE []
      eqclauses = map mkcon cs ++ [def]
      eq = Method (name "==") (clauseE eqclauses)
  in InstD loc ctx cls [eq]

-- Derive an instance of Free (before flattening and inference) for the given
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
deriveFree loc ctx cls cs =
  let -- name of tag for constructor: (isFooX :: Bool)
      mkTag :: ConRec -> Sig
      mkTag (NormalC nm _) = Sig (name "is" `nappend` nm) boolT
      mkTag (RecordC nm ts) = mkTag (NormalC nm (map snd ts))

      -- fields for constructor: [a1FooX, a2FooX, ...]
      mkFields :: ConRec -> [Sig]
      mkFields (NormalC nm ts)
        = [Sig (name ('a' : show i) `nappend` nm) UnknownT | i <- [1..length ts]]
      mkFields (RecordC nm ts) = mkFields (NormalC nm (map snd ts))

      -- application of constructor to its fields:
      --    FooX a1FooX a2FooX ...
      mkCon :: ConRec -> Exp
      mkCon c@(NormalC nm _) =
        let fields = mkFields c
        in appsE (conE (Sig nm UnknownT)) (map varE fields)
      mkCon (RecordC nm ts) = mkCon (NormalC nm (map snd ts))

      -- given [t1, t2, ..., tnm1] [v1, v2, ... vn]
      --  Generates the nested if statement:
      --    if t1 then v1
      --          else if t2 then v2
      --                     else if ...
      --                             else vn
      mkIf :: [Sig] -> [Exp] -> Exp
      mkIf [] [x] = x
      mkIf (t:ts) (x:xs) = ifE (varE t) x (mkIf ts xs)
      mkIf ss es = error $ "mkIf: " ++ show ss ++ ", " ++ show es

      tags = map mkTag (init cs)
      fields = concat (map mkFields cs)
      freevars = [BindS (VarP n) (VarE (Sig (name "free") UnknownT))
                    | Sig n _ <- tags ++ fields]
      bodies = map mkCon cs
      value = mkIf tags bodies
      rtn = NoBindS $ appE (varE (Sig (name "return") UnknownT)) value
      stmts = freevars ++ [rtn]
      free = Method (name "free") (doE stmts)
  in InstD loc ctx cls [free]

-- Derive an instance of Show for the given data type declaration.
-- Generates something of the form:
--    instance ctx => Show (Foo ...) where
--       show (Foo1 a1 a2 ...) = show_helper ["Foo1", show a1, show a2, ...]
--       show (Foo2 a1 a2 ...) = show_helper ["Foo2", show a1, show a2, ...]
--            ...
--       show (FooN a1 a2 ...) = show_helper ["FooN", show a1, show a2, ...]
deriveShow :: Location -> Context -> Class -> [ConRec] -> Dec
deriveShow loc ctx cls cs =
  let mkcon :: ConRec -> MAlt
      mkcon (NormalC cn ts) =
        let fields = [Sig (name $ 'a' : show i) UnknownT | i <- [1..length ts]]
            p = ConP cn [VarP n | Sig n _ <- fields]
            shows = [appE (VarE (Sig (name "show") UnknownT)) (VarE a) | a <- fields]
            body = appE (VarE (Sig (name "__show_helper") UnknownT)) $
                     listE (stringE (unname cn) : shows)
        in simpleMA [p] body []
      mkcon (RecordC cn ts) = mkcon (NormalC cn (map snd ts))

      shclauses = map mkcon cs
      sh = Method (name "show") (clauseE shclauses)
  in InstD loc ctx cls [sh]

derive :: Location -> Context -> Class -> [ConRec] -> Dec
derive loc ctx cls@(Class n _)
 | n == name "Eq" = deriveEq loc ctx cls
 | n == name "Free" = deriveFree loc ctx cls
 | n == name "Show" = deriveShow loc ctx cls
 | otherwise = error $ "deriving " ++ show n ++ " not supported in smten"
      
iderive :: Location -> Name -> Name -> [TyVar] -> [ConRec] -> Dec
iderive loc n dn vars cs = 
  let dt = appsT (conT dn) (map tyVarType vars)
      cls = Class n [dt]
    
      keep :: Type -> Bool
      keep t = not $ or [null (varTs t), isSubType dt t]

      fields :: ConRec -> [Type]
      fields (NormalC _ ts) = ts
      fields (RecordC _ ts) = map snd ts

      fieldts = concatMap fields cs
      ctx = nub [Class n [t] | t <- filter keep fieldts]
  in derive loc ctx cls cs

