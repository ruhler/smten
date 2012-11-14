
module Seri.Dec.Record (
    ConRec(..), recordD, recordC, recordU,
    ) where

import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Fresh
import Seri.Exp
import Seri.Dec.Dec

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
recordD :: Name -> [TyVar] -> [ConRec] -> [String] -> [Dec]
recordD nm vars cons derivings =
  let mkcon :: ConRec -> Con
      mkcon (NormalC n ts) = Con n ts
      mkcon (RecordC n ts) = Con n (map snd ts)

      dt = appsT (ConT nm) (map tyVarType vars)

      mkundef :: Con -> Dec
      mkundef (Con n ts) =
        let undefnm = (record_undefnm n)
            undefet = arrowsT $ ts ++ [dt]
            undefe = appsE (ConE (Sig n undefet)) [VarE (Sig (name "undefined") t) | t <- ts]
        in ValD (TopSig undefnm [] dt) undefe

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
                  body = mlamE $ MMatch [pat] (varE (Sig (name "x") t))
              in ValD (TopSig n [] at) body
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
                  body = mlamE $ MMatch [VarP n, mypat] myexp
              in ValD (TopSig (record_updnm n) [] ut) body
        in map mkupd (zip ts [0..])
                            
      cons' = map mkcon cons
      undefs = map mkundef cons'
      accs = concatMap mkaccs cons
      upds = concatMap mkupds cons
      derivations = [derive d nm vars cons' | d <- derivings]
  in concat [[DataD nm vars cons'], derivations, undefs, accs, upds]

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
--    instance (Eq a, Eq b, ...) => Eq (Foo a b ...) where
--       (==) (Foo1 a1 a2 ...) (Foo1 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) (Foo2 a1 a2 ...) (Foo2 b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--            ...
--       (==) (FooN a1 a2 ...) (FooN b1 b2 ...) = and [a1 == b1, a2 == b2, ...]
--       (==) _ _ = False
--
--       (/=) = (/=#)
deriveEq :: Name -> [TyVar] -> [Con] -> Dec
deriveEq dn vars cs =
  let dt = appsT (ConT dn) (map tyVarType vars)
        
      mkcon :: Con -> MMatch
      mkcon (Con cn ts) =
        let fieldsA = [Sig (name $ 'a' : show i) t | (t, i) <- zip ts [1..]]
            fieldsB = [Sig (name $ 'b' : show i) t | (t, i) <- zip ts [1..]]
            pA = ConP cn [VarP n | Sig n _ <- fieldsA]
            pB = ConP cn [VarP n | Sig n _ <- fieldsB]
            body = appsE (VarE (Sig (name "and") UnknownT))
                    [listE [appsE (VarE (Sig (name "==") UnknownT)) 
                               [VarE a, VarE b] | (a, b) <- zip fieldsA fieldsB]]
        in MMatch [pA, pB] body

      def = MMatch [WildP, WildP] falseE
      ctx = [Class (name "Eq") [tyVarType c] | c <- vars]
      eqclauses = map mkcon cs ++ [def]
      eq = Method (name "==") (clauseE eqclauses)
      ne = Method (name "/=") (varE (Sig (name "/=#") UnknownT))
  in InstD ctx (Class (name "Eq") [dt]) [eq, ne]
    

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
-- instance (Free a, Free b, ...) => Free (Foo a b ...) where
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
deriveFree :: Name -> [TyVar] -> [Con] -> Dec
deriveFree dn vars cs =
  let dt = appsT (ConT dn) (map tyVarType vars)
    
      -- name of tag for constructor: (isFooX :: Bool)
      mkTag :: Con -> Sig
      mkTag (Con nm _) = Sig (name "is" `nappend` nm) boolT

      -- fields for constructor: [a1FooX, a2FooX, ...]
      mkFields :: Con -> [Sig]
      mkFields (Con nm ts)
        = [Sig (name ('a' : show i) `nappend` nm) t | (t, i) <- zip ts [1..]]

      -- application of constructor to its fields:
      --    FooX a1FooX a2FooX ...
      mkCon :: Con -> Exp
      mkCon c@(Con nm ts) =
        let fields = mkFields c
        in appsE (conE (Sig nm UnknownT)) (map varE fields)

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
      ctx = [Class (name "Free") [tyVarType c] | c <- vars]
  in InstD ctx (Class (name "Free") [dt]) [free]
      
derive :: String -> Name -> [TyVar] -> [Con] -> Dec
derive "Eq" = deriveEq
derive "Free" = deriveFree
derive x = error $ "deriving " ++ show x ++ " not supported in seri"

