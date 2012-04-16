
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations (
    SeriDec(..),
    declname, declidname,
    declval', declcon', decltype',
    declprim, declval, declcon, decltype, declcommit,
    ) where

import Data.Char(isUpper)
import Language.Haskell.TH

import Seri.THUtils
import Seri.InstId
import qualified Seri.IR as SIR
import qualified Seri.Typed as S

class SeriDec a where
    dec :: a -> SIR.Dec

name_X :: String -> Name -> Name
name_X pre x = mkName $ pre ++ nameBase x

-- The name of the (possibly) polymorphic function generated.
declname :: Name -> Name
declname = name_X "_seriP_"

declidname :: Name -> Name
declidname = name_X "_seriI_"

-- The name of the declaration type
name_D :: Name -> Name
name_D = name_X "SeriDec_"

name_DD :: Name -> Name
name_DD = name_X "SeriDecD_"

declprim :: SIR.Name -> Q Type -> Q [Dec]
declprim nm ty = declval nm ty [e| S.primitive $(litE (StringL nm)) |]

declcon :: String -> Q Type -> Q [Dec]
declcon n qt = do
    t <- qt
    return $ declcon' (mkName n) t

declval :: String -> Q Type -> Q Exp -> Q [Dec]
declval n qt qe = do
    t <- qt
    e <- qe
    return $ declval' (mkName n) t e

-- declval' name ty exp
-- Make a seri value declaration
--   name - name of the seri value being defined.
--   ty - the polymorphic haskell type of the expression.
--   exp - the value
--
-- For (contrived) example, given:
--  name: foo
--  ty: (Eq a) => a -> Integer
--  exp: lamE "x" (\x -> appE (varE "incr") (integerE 41))
--  iscon: False
--
-- The following haskell declarations are generated (approximately):
--  _seriP_foo :: (Eq a, SeriType a) => Typed Exp (a -> Integer)
--  _seriP_foo = lamE "x" (\x -> appE (varE "incr") (integerE 41))
--
--  _seriI_foo :: Typed Exp (a -> Integer) -> InstId
--  _seriI_foo _ = noinst
--
--  data SeriDec_foo = SeriDec_foo
--
--  instance SeriDec SeriDec_foo where
--      dec _ = valD "foo" (_seri_foo :: Typed Exp (VarT_a -> Integer))
declval' :: Name -> Type -> Exp -> [Dec]
declval' n t e =
  let dt = declize t
      sig_P = SigD (declname n) dt
      impl_P = FunD (declname n) [Clause [] (NormalB e) []]

      sig_I = SigD (declidname n) (declidize t)
      impl_I = FunD (declidname n) [Clause [WildP] (NormalB $ VarE 'noinst) []]

      data_D = DataD [] (name_D n) [] [NormalC (name_D n) []] []

      body = apply 'S.valD [string n, SigE (VarE (declname n)) (concretize dt)]
      impl_D = FunD 'dec [Clause [WildP] (NormalB body) []]
      inst_D = InstanceD [] (AppT (ConT ''SeriDec) (ConT $ name_D n)) [impl_D]
  in [sig_P, impl_P, sig_I, impl_I, data_D, inst_D]

-- declcon' name ty
-- Make a seri data constructor declaration
--   name - name of the seri data constructor being defined.
--   ty - the polymorphic haskell type of the expression.
--
-- For (contrived) example, given:
--  name: Foo
--  ty: a -> Bar
--
-- The following haskell declarations are generated (approximately):
--  _seri_Foo :: (SeriType a) => Typed Exp (a -> Bar)
--  _seri_Foo = conE' "Foo"
declcon' :: Name -> Type -> [Dec]
declcon' n t =
  let dt = declize t
      sig_P = SigD (declname n) dt
      impl_P = FunD (declname n) [Clause [] (NormalB (apply 'S.conE' [string n])) []]
  in [sig_P, impl_P]

-- decltype' 
-- Given a type declaration, make a seri type declaration for it, assuming the
-- type is already defined in haskell.
--
-- For example, given 
--    data Foo a = Bar Integer
--               | Sludge a
--
-- The following haskell declarations are generated:
--    instance SeriType1 Foo where
--       seritype1 _ = ConT "Foo"
--
--    data SeriDecD_Foo = SeriDecD_Foo
--
--    instance SeriDec SeriDecD_Foo where
--        dec _ = DataD "Foo" ["a"] [Con "Bar" [ConT "Integer"],
--                                   Con "Sludge" [ConT "VarT_a"]]
--
--    _seri_Bar :: Typed Exp (Integer -> Foo)
--    _seri_Bar = conE "Bar"
--
--    _seri_Sludge :: (SeriType a) => Typed Exp (a -> Foo)
--    _seri_Sludge = conE "Sludge"
--
-- Record type constructors are also supported, in which case the selector
-- functions will also be declared like normal seri values.
--
decltype' :: Dec -> [Dec]
decltype' (DataD [] dt vars cs _) =
 let numvars = length vars
     classname = "SeriType" ++ if numvars == 0 then "" else show numvars
     methname = "seritype" ++ if numvars == 0 then "" else show numvars
     vnames = map (\(PlainTV n) -> n) vars
     dtapp = appts $ (ConT dt):(map VarT vnames)

     -- Assuming the data type is polymorphic in type variables a, b, ...
     -- Given type t, return type (forall a b ... . t)
     --
     contextify :: Type -> Type
     contextify t = ForallT vars [] t

     -- contype: given the list of field types [a, b, ...] for a constructor
     -- form the constructor type: a -> b -> ... -> Foo
     contype :: [Type] -> Type
     contype ts = contextify $ arrowts (ts ++ [dtapp])

     -- produce the declarations needed for a given constructor.
     mkcon :: Con -> [Dec]
     mkcon (NormalC nc sts) =
        let ty = contype (map snd sts)
        in declcon' nc ty
     mkcon (RecC nc sts) =
        let ty = contype (map (\(_, _, t) -> t) sts)
            constrs = declcon' nc ty
            numfields = toInteger $ length sts

            mkacc :: Integer -> Name -> Type -> [Dec]
            mkacc i n st =
                let t = contextify $ arrowts [dtapp, st]
                    e = apply 'S.selector [string dt, integer i, integer numfields]
                in declval' n t e

            accessors = concat $ map (\(i, (n, _, t)) -> mkacc i n t) (zip [0..] sts)
        in constrs ++ accessors

     stimpl = FunD (mkName methname) [Clause [WildP] (NormalB (AppE (ConE 'SIR.ConT) (string dt))) []]
     stinst = InstanceD [] (AppT (ConT (mkName classname)) (ConT dt)) [stimpl]

     data_D = DataD [] (name_DD dt) [] [NormalC (name_DD dt) []] []


     -- Given a type, return an expression corresonding to the seri type of
     -- that type.
     mktyinfo :: Type -> Exp
     mktyinfo t = apply 'S.seritype [SigE (VarE 'undefined) (concretize t)]

     -- Given a constructor, return an expression corresponding to the Seri
     -- Con representing that constructor.
     mkconinfo :: Con -> Exp
     mkconinfo (NormalC n sts)
        = applyC 'SIR.Con [string n, ListE (map (\(_, t) -> mktyinfo t) sts)]
     mkconinfo (RecC n sts)
        = applyC 'SIR.Con [string n, ListE (map (\(_, _, t) -> mktyinfo t) sts)]

     body = applyC 'SIR.DataD [string dt, ListE (map string vnames), ListE (map mkconinfo cs)]
     sdimpl = FunD 'dec [Clause [WildP] (NormalB body) []]
     sdinst = InstanceD [] (AppT (ConT ''SeriDec) (ConT $ name_DD dt)) [sdimpl]

     constrs = concat $ map mkcon cs
 in concat [[stinst, data_D, sdinst], constrs]

decltype :: Name -> Q [Dec]
decltype nm = do
    TyConI d <- reify nm
    return $ decltype' d

-- Given the raw haskell type corresponding to an expression, return the type
-- of the haskell function representing an expression of that type.
--
-- For example
--  input: (Eq a) => a -> Integer
--  output: (Eq a, SeriType a) => Typed Exp (a -> Integer) 
declize :: Type -> Type
declize ty = 
  let typedexp t = (AppT (AppT (ConT ''S.Typed) (ConT ''SIR.Exp)) t)

      -- Given a type variable, figure out what predicate we should add for it
      -- in the context.
      --
      -- TODO: this is a bad special case hack. Can we come up with a better
      -- way for figuring out the kind of each type variable?
      stcon :: TyVarBndr -> Pred
      stcon (PlainTV x) | 'm' == head (nameBase x)
        = ClassP ''S.SeriType1 [VarT x]
      stcon (PlainTV x) = ClassP ''S.SeriType [VarT x]

  in case ty of
        ForallT vns c t ->
           let ctx = map stcon vns
           in ForallT vns (c ++ ctx) (typedexp t)
        _ -> typedexp ty

-- Given the raw haskell type corresponding to an expression, return the type
-- of the haskell function representing the InstId of that expression.
--
-- For example
--  input: (Eq a) => a -> Integer
--  output: Typed Exp (a -> Integer) -> InstId
declidize :: Type -> Type 
declidize ty =
  let mkt t = arrowts [(AppT (AppT (ConT ''S.Typed) (ConT ''SIR.Exp)) t), (ConT ''InstId)]
  in case ty of
     ForallT vns c t -> ForallT vns [] (mkt t)
     _ -> mkt ty

-- Given a potentially polymorphic haskell type, convert it to a concrete
-- haskell type which represents the polymorphic seri type.
--
-- In other words, replace all occurences of VarT "foo" with VarT_foo.
concretize :: Type -> Type
concretize (ForallT _ _ t) = concretize t
concretize (VarT nm) = ConT $ mkName ("VarT_" ++ (nameBase nm))
concretize (AppT a b) = AppT (concretize a) (concretize b)
concretize t = t

-- Declarations may not be seen right away. Call this template haskell
-- function to force the declarations to be committed.
--
-- So, for example, to use this you would declare all your seri functions,
-- then below those in the source file call this as a top level template
-- haskell slice, then below that in the source file you can use quoted seri
-- expressions referring to the declarations.
declcommit :: Q [Dec]
declcommit = return []

