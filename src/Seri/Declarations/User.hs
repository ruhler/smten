
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations.User (
    declprim, declcon, declval,
    decltycon, decltyvar, decltyvars, decltype,
    declclass, declvartinst,
    declcommit,
    ) where

import Language.Haskell.TH

import Seri.Declarations.Library
import Seri.Declarations.Names
import Seri.Typed(primitive)

-- API for users to declare seri things.

-- declprim name type
-- Declare a Seri primitive.
--   name - the name of the primitive.
--   type - the type of the primitive.
declprim :: String -> Q Type -> Q [Dec]
declprim nm ty = declval nm ty [e| primitive $(litE (StringL nm)) |]

-- declcon name type
-- Declare a Seri constructor
--   name - the name of the constructor
--   type - the type of the constructor
declcon :: String -> Q Type -> Q [Dec]
declcon n qt = do
    t <- qt
    return $ declcon' (mkName n) t

-- declval name type value
-- Declare a Seri value.
--   name - the name of the value
--   type - the type of the value
--   value - the value of the value. The given expression should have haskell
--           type (Typed Exp <type>).
declval :: String -> Q Type -> Q Exp -> Q [Dec]
declval n qt qe = do
    t <- qt
    e <- qe
    return $ declval' (mkName n) t e

-- decltycon kind name
--
-- Declare a primitive type constructor.
--   kind - the kind of the type constructor. That is, the number of type
--          arguments it takes.
--   name - the name of the type constructor.
decltycon :: Integer -> Name -> Q [Dec]
decltycon k nm = return $ decltycon' k nm

-- decltyvar kind name
--
-- Declare a type variable.
--   name - the name of the type variable.
decltyvar :: String -> Q [Dec]
decltyvar nm = return $ decltyvar' nm

-- Declare all the type variables specified in Declarations.Polymorphic
decltyvars :: [(Integer, [String])] -> Q [Dec]
decltyvars [] = return []
decltyvars ((i, ns):xs) = do
    dns <- decltyvars xs
    di <- mapM decltyvar ns
    return $ concat (dns:di)

-- decltype name
-- Declare a Seri type based on an existing haskell type.
--   name - the name of the haskell type to import into Seri.
decltype :: Name -> Q [Dec]
decltype nm = do
    TyConI d <- reify nm
    return $ decltype' d

-- declclass name
-- Declare a Seri class based on an existing haskell class.
--   name - the name of the haskell class to import into Seri.
declclass :: Name -> Q [Dec]
declclass nm = do
    ClassI d _ <- reify nm
    return $ declclass' d

-- declvartinst class vart
--   Declare a dummy instance of a seri class with the given name for the
--   given variable type.
declvartinst :: String -> [String] -> Q [Dec]
declvartinst n v = do
    ClassI d _ <- reify (classname (mkName n))
    return $ declvartinst' d v

-- Declarations may not be seen right away. Call this template haskell
-- function to force the declarations to be committed.
--
-- So, for example, to use this you would declare all your seri functions,
-- then below those in the source file call this as a top level template
-- haskell splice, then below that in the source file you can use quoted seri
-- expressions referring to the declarations.
declcommit :: Q [Dec]
declcommit = return []

