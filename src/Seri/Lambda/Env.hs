
{-# LANGUAGE FlexibleInstances #-}

-- | Definitions and utilities for working with seri environments. An
-- environment captures an object in the context of a bunch of seri
-- declarations.
module Seri.Lambda.Env (
    Env(), val, mkenv, withenv, decls,
    lookupvar, lookupDataD, lookupClassD,
    lookupDataConstructor, lookupVarInfo,
    minimize, sort,
    ) where

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
      theValD (ValD (Sig nm _) _) = n == nm
      theValD _ = False
  in theOneOf "ValD" n theValD decls
        
-- | Look up a DataD with given Name in the given Environment.
lookupDataD :: Env a -> Name -> Failable Dec
lookupDataD (Env decls _) n =
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
lookupInstD :: Env a -> Name -> [Type] -> Failable Dec
lookupInstD (Env decls _) n t =
  let theInstD :: Dec -> Bool
      theInstD (InstD (Class nm ts) _) = n == nm && t == ts
      theInstD _ = False
  in theOneOf "InstD" n theInstD decls

-- | Look up the type of a method in the given class.
lookupSig :: Env a -> Name -> Name -> Failable Type
lookupSig e cls meth =
  let sigInClass :: [Sig] -> Failable Type
      sigInClass [] = fail $ "method " ++ meth ++ " not found in class " ++ cls
      sigInClass ((Sig n t):_) | n == meth = return t
      sigInClass (_:xs) = sigInClass xs
  in do
     ClassD _ _ sigs <- lookupClassD e cls
     sigInClass sigs


-- | Given a VarE in an environment return the value of that variable as
-- determined by the environment.
--
-- Fails if the variable could not be found in the environment.
lookupvar :: Env Exp -> Failable (Type, Exp)
lookupvar e@(Env _ (VarE (Sig n _) Declared)) = do
  (ValD (Sig _ t) v) <- lookupValD e n
  return (t, v)
lookupvar e@(Env _ (VarE (Sig x _) (Instance (Class n ts)))) =
  let mlook :: [Method] -> Failable (Type, Exp)
      mlook [] = fail $ "method " ++ x ++ " not found"
      mlook ((Method nm body):ms) | nm == x = do
          t <- lookupSig e n x
          return (t, body)
      mlook (m:ms) = mlook ms
  in do
      InstD _ ms <- lookupInstD e n ts  
      mlook ms
lookupvar e@(Env _ v) = error $ "lookupvar: " ++ pretty v

union :: (Eq a) => [a] -> [a] -> [a]
union a b = nub $ a ++ b

-- declarations env x
-- Return the set of declarations in the given environment a thing depends on.
declarations :: (Data a) => [Dec] -> a -> [Dec]
declarations m =
  let theenv = Env m ()
      qexp :: Exp -> [Dec]
      qexp (VarE (Sig n _) Declared) = attemptM $ lookupValD theenv n
      qexp (VarE (Sig n _) (Instance (Class ni ts))) = catMaybes [attemptM $ lookupClassD theenv ni, attemptM $ lookupInstD theenv ni ts]
      qexp e = []

      qtype :: Type -> [Dec]
      qtype (ConT n) = attemptM $ lookupDataD theenv n
      qtype t = []

      qclass :: Class -> [Dec]
      qclass (Class n ts) = catMaybes [attemptM $ lookupClassD theenv n, attemptM $ lookupInstD theenv n ts]

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
lookupDataConstructor :: Env Name -> Failable Type
lookupDataConstructor (Env _ "True") = return $ ConT "Bool"
lookupDataConstructor (Env _ "False") = return $ ConT "Bool"
lookupDataConstructor (Env _ "[]") = return $ listT (VarT "a")
lookupDataConstructor (Env _ ":") = return $ arrowsT [VarT "a", AppT (ConT "[]") (VarT "a"), AppT (ConT "[]") (VarT "a")]
lookupDataConstructor (Env _ "()") = return $ ConT "()"
lookupDataConstructor (Env _ "(,)") = return $ arrowsT [VarT "a", VarT "b", AppT (AppT (ConT "(,)") (VarT "a")) (VarT "b")]
lookupDataConstructor (Env _ "(,,)") = return $ arrowsT [VarT "a", VarT "b", VarT "c",  AppT (AppT (AppT (ConT "(,,)") (VarT "a")) (VarT "b")) (VarT "c")]
lookupDataConstructor (Env decs n) = 
    case catMaybes [typeofCon d n | d <- decs] of
        [] -> fail $ "data constructor " ++ n ++ " not found in env"
        [x] -> return x
        xs -> fail $ "multiple data constructors with name " ++ n ++ " found in env"

-- | Look up VarInfo for the variable with given signature.
-- Returns UnknownVI if the variable is not bound or declared.
lookupVarInfo :: Env Sig -> VarInfo
lookupVarInfo e@(Env ds (Sig n t))
  = case (attemptM $ lookupValD e n) of
        Just _ -> Declared
        Nothing -> error $ "TODO: lookupVarInfo which isn't declared"

