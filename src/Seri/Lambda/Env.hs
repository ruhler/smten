
{-# LANGUAGE FlexibleInstances #-}

-- | Definitions and utilities for working with seri environments. An
-- environment captures an object in the context of a bunch of seri
-- declarations.
module Seri.Lambda.Env (
    Env(), val, mkenv, withenv, decls,
    VarInfo(..),
    minimize, sort,
    lookupVarType, lookupVarValue, lookupVar, lookupVarInfo,
    lookupMethodType,
    lookupDataD, lookupDataConType,
    ) where

import Debug.Trace

import Data.Generics
import Data.List(nub, partition)
import Data.Maybe

import Seri.Failable
import Seri.Lambda.IR
import Seri.Lambda.Ppr
import Seri.Lambda.Types

data Env x = Env {
    env :: [Dec],
    val :: x
} deriving (Show, Eq)

instance (Ppr a) => Ppr (Env a) where
    ppr (Env ds x) = ppr ds $+$ ppr x

-- | 'VarInfo' 
-- Information about a variable.
-- [@Bound@] The variable is locally bound by a lambda or pattern match.
-- [@Primitive@] The variable is a primitive.
-- [@Declared@] The variable refers to a top level declaration.
-- [@Instance@] The variable refers to a method of the given class instance.
data VarInfo = Bound | Primitive |  Declared | Instance Class
    deriving (Eq, Show)


-- | Construct an environment for 'x' with the given declarations.
mkenv :: [Dec] -> x -> Env x
mkenv ds x = Env ds x

-- | Make an environment for an object 'b' which is the same as the given
-- environment for object 'a'.
withenv :: Env a -> b -> Env b
withenv (Env m _) x = Env m x

-- | Extract the declarations from an environment.
decls :: Env x -> [Dec]
decls x = env x

-- | theOneOf kind name predicate decls
-- Common code for extracting a single declaration from a list.
--  kind - the kind of declaration searched in (for error messages)
--  name - the name of the declaration searched for (for error messages)
--  predicate - a predicate which identifies the desired declaration
--  decls - the declarations to search in.
theOneOf :: String -> String -> (Dec -> Bool) -> [Dec] -> Failable Dec
theOneOf kind n p ds =
    case filter p ds of
        [x] -> return x
        [] -> fail $ kind ++ " for " ++ n ++ " not found"
        xs -> fail $ "Multiple definitions for " ++ n ++ " found: " ++ concatMap pretty xs


-- | Look up a ValD with given Name in the given Environment.
lookupValD :: Env a -> Name -> Failable Dec
lookupValD (Env decls _) n =
  let theValD :: Dec -> Bool
      theValD (ValD (TopSig nm _ _) _) = n == nm
      theValD _ = False
  in theOneOf "ValD" n theValD decls

-- | Look up a PrimD with given Name in the given Environment.
lookupPrimD :: Env Name -> Failable Dec
lookupPrimD (Env decls n) =
  let thePrimD :: Dec -> Bool
      thePrimD (PrimD (TopSig nm _ _)) = n == nm
      thePrimD _ = False
  in theOneOf "PrimD" n thePrimD decls
 
        
-- | Look up a DataD with given type constructor Name in the given
-- Environment.
lookupDataD :: Env Name -> Failable Dec
lookupDataD (Env decls n) =
  let theDataD :: Dec -> Bool
      theDataD (DataD nm _ _) = n == nm
      theDataD _ = False
  in theOneOf "DataD" n theDataD decls

-- | Look up a ClassD with given Name in the given Environment.
lookupClassD :: Env a -> Name -> Failable Dec
lookupClassD (Env decls _) n =
  let theClassD :: Dec -> Bool
      theClassD (ClassD nm _ _) = n == nm
      theClassD _ = False
  in theOneOf "ClassD" n theClassD decls

-- | Look up an InstD in the given Environment.
lookupInstD :: Env a -> Class -> Failable Dec
lookupInstD (Env decls _) (Class n t) =
  let theInstD :: Dec -> Bool
      theInstD (InstD (Class nm ts) _) = n == nm && t == ts
      theInstD _ = False
  in theOneOf "InstD" (pretty (Class n t)) theInstD decls

-- | Look up the type of a method in the given class.
lookupSig :: Env a -> Name -> Name -> Failable Type
lookupSig e cls meth =
  let sigInClass :: [TopSig] -> Failable Type
      sigInClass [] = fail $ "method " ++ meth ++ " not found in class " ++ cls
      sigInClass ((TopSig n _ t):_) | n == meth = return t
      sigInClass (_:xs) = sigInClass xs
  in do
     ClassD _ _ sigs <- lookupClassD e cls
     sigInClass sigs


-- | Given a VarE in an environment return the polymorphic type and value of
-- that variable as determined by the environment. For methods, the type
-- returned is that of the class definition, not for any specific instance.
--
-- Fails if the variable could not be found in the environment.
lookupVar :: Env Sig -> Failable (Type, Exp)
lookupVar e@(Env _ s@(Sig n _)) = 
    case (lookupVarInfo e) of
        Declared -> do
            (ValD (TopSig _ _ t) v) <- lookupValD e n
            return (t, v)
        (Instance cls@(Class cn ts)) ->
            let mlook :: [Method] -> Failable (Type, Exp)
                mlook [] = fail $ "method " ++ n ++ " not found"
                mlook ((Method nm body):ms) | nm == n = do
                    t <- lookupSig e cn n
                    return (t, body)
                mlook (m:ms) = mlook ms
            in do
                InstD _ ms <- lookupInstD e cls
                mlook ms
        _ -> fail $ "lookupVar: " ++ n ++ " not found in environment"

-- | Look up the value of a variable in an environment.
lookupVarValue :: Env Sig -> Failable Exp
lookupVarValue s = lookupVar s >>= return . snd

-- | Given a VarE in an environment, return the polymorphic type of that
-- variable as determined by the environment. For methods of classes, the
-- type returned is that in the class definition, not for any specific class
-- instance.
--
-- Fails if the variable could not be found in the environment.
lookupVarType :: Env Name -> Failable Type
lookupVarType e@(Env ds n)  = do
    case (attemptM $ lookupValD e n, attemptM $ lookupPrimD e) of
       (Just (ValD (TopSig _ _ t) _), _) -> return t
       (_, Just (PrimD (TopSig _ _ t))) -> return t
       _ ->
          let getSig :: Dec -> Maybe Type
              getSig (ClassD cn _ sigs) =
                case filter (\(TopSig sn _ _) -> sn == n) sigs of
                    [] -> Nothing
                    [TopSig _ _ t] -> Just t
              getSig _ = Nothing

              answer = listToMaybe (catMaybes (map getSig ds))
          in case answer of
                Just t -> return t
                Nothing -> fail $ "lookupVarType: '" ++ n ++ "' not declared"

-- | Given the name of a method and a specific class instance for the method,
-- return the type of that method for the specific instance.
lookupMethodType :: Env Name -> Class -> Failable Type
lookupMethodType e@(Env ds n) (Class cn ts) = do
    t <- lookupVarType e
    ClassD _ vars _ <- lookupClassD (mkenv ds ()) cn
    return $ assign (zip vars ts) t

union :: (Eq a) => [a] -> [a] -> [a]
union a b = nub $ a ++ b

-- declarations env x
-- Return the set of declarations in the given environment a thing depends on.
declarations :: (Data a) => [Dec] -> a -> [Dec]
declarations m =
  let theenv = Env m ()
      qexp :: Exp -> [Dec]
      qexp (VarE s@(Sig n _)) =
         case (lookupVarInfo (withenv theenv s)) of
            Declared -> attemptM $ lookupValD theenv n
            (Instance cls@(Class ni ts)) ->
                catMaybes [attemptM $ lookupClassD theenv ni,
                             attemptM $ lookupInstD theenv cls]
            Bound -> []
      qexp e = []

      qtype :: Type -> [Dec]
      qtype (ConT n) = attemptM $ lookupDataD (withenv theenv n)
      qtype t = []

      qclass :: Class -> [Dec]
      qclass cls@(Class n ts) = catMaybes [
            attemptM $ lookupClassD theenv n,
            attemptM $ lookupInstD theenv cls]

      query :: (Typeable a) => a -> [Dec]
      query = extQ (extQ (mkQ [] qexp) qtype) qclass
  in everything union query

-- | Minimize an environment.
-- Remove any declarations in the environment not needed by the object in the
-- environment.
minimize :: (Data a) => Env a -> Env a
minimize (Env m x) =
  let alldecls :: [Dec] -> [Dec]
      alldecls d =
        let ds = declarations m d
            dds = d `union` ds
        in if (length d == length dds)
            then d
            else alldecls dds
  in Env (alldecls (declarations m x)) x

-- | sort ds
-- Perform a topological sort of declarations ds by dependency.
--   returns (sorted, mutual)
--  sorted - the list of sorted declarations. Declarations earlier in the list
--           do not depend on declarations later in the list.
--  mutual - a list of the remaining mutually dependent declarations from ds.
sort :: [Dec] -> ([Dec], [Dec])
sort ds = 
  let dependencies :: [(Dec, [Dec])]
      dependencies = [(d, declarations ds d) | d <- ds]

      sorte :: [Dec] -> [(Dec, [Dec])] -> ([Dec], [Dec])
      sorte sorted unsorted = 
        let indep :: (Dec, [Dec]) -> Bool
            indep (d, deps) = and [dp `elem` sorted | dp <- deps]
        in case partition indep unsorted of
            ([], deps) -> (sorted, map fst unsorted)
            (indeps, deps) -> sorte (sorted ++ (map fst indeps)) deps
  in sorte [] dependencies

-- | Given the name of a data constructor in the environment, return its type.
lookupDataConType :: Env Name -> Failable Type
lookupDataConType (Env _ "True") = return $ ConT "Bool"
lookupDataConType (Env _ "False") = return $ ConT "Bool"
lookupDataConType (Env _ "[]") = return $ listT (VarT "a")
lookupDataConType (Env _ ":") = return $ arrowsT [VarT "a", AppT (ConT "[]") (VarT "a"), AppT (ConT "[]") (VarT "a")]
lookupDataConType (Env _ "()") = return $ ConT "()"
lookupDataConType (Env _ "(,)") = return $ arrowsT [VarT "a", VarT "b", AppT (AppT (ConT "(,)") (VarT "a")) (VarT "b")]
lookupDataConType (Env _ "(,,)") = return $ arrowsT [VarT "a", VarT "b", VarT "c",  AppT (AppT (AppT (ConT "(,,)") (VarT "a")) (VarT "b")) (VarT "c")]
lookupDataConType (Env _ "(,,,)")
    = return $ arrowsT [VarT "a", VarT "b", VarT "c", VarT "d",
         AppT (AppT (AppT (AppT (ConT "(,,,)") (VarT "a")) (VarT "b")) (VarT "c")) (VarT "l")]
lookupDataConType (Env decs n) = 
    case catMaybes [typeofCon d n | d <- decs] of
        [] -> fail $ "data constructor " ++ n ++ " not found in env"
        [x] -> return x
        xs -> fail $ "multiple data constructors with name " ++ n ++ " found in env"

-- | Look up VarInfo for the variable with given signature.
-- Returns Bound if the variable is not declared or an instance or primitive.
lookupVarInfo :: Env Sig -> VarInfo
lookupVarInfo e@(Env ds (Sig n t))
  = case (attemptM $ lookupValD e n, attemptM $ lookupPrimD (withenv e n)) of
        (Just _, _) -> Declared
        (_, Just _) -> Primitive
        _ ->
          let getSig :: Dec -> Maybe (Name, [Name], Type)
              getSig (ClassD cn cts sigs) =
                case filter (\(TopSig sn _ _) -> sn == n) sigs of
                    [] -> Nothing
                    [TopSig _ _ t] -> Just (cn, cts, t)
              getSig _ = Nothing

              answer = listToMaybe (catMaybes (map getSig ds))
          in case answer of 
              Nothing -> Bound
              Just (cn, cts, st) ->
                 let assigns = assignments st t
                     cts' = assign assigns (map VarT cts)
                 in Instance (Class cn cts')

