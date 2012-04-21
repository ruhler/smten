
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations.Utils (
    tvarkind, stpred, valuetype, instidtype, texpify, concrete, concrete', seritypeexp,
    ) where

import Language.Haskell.TH

import Seri.THUtils
import qualified Seri.Typed as S
import qualified Seri.IR as SIR
import Seri.Declarations.Names

-- Return the kind of a type variable.
--  0 means kind *
--  1 means kind * -> *
--  2 means kind (* -> *) -> * 
--  etc...
--
-- We assume the following:
--   - PlainTV's starting with 'a', 'b', 'c', or 'd' are of kind *
--   - PlainTV's starting with 'm' are of kind * -> *
-- Note: this should be kept in sync with the info in Seri.Polymorphic
tvarkind :: TyVarBndr -> Integer
tvarkind (PlainTV v) | head (nameBase v) `elem` "abcd" = 0
tvarkind (PlainTV v) | head (nameBase v) `elem` "m" = 1
tvarkind (KindedTV v StarK) = 0
tvarkind (KindedTV v (ArrowK StarK StarK)) = 1
tvarkind v = error $ "TODO: Seri.Declarations.Utils.tvarkind " ++ show v

-- Given a type variable, figure out what predicate we should add for it
-- in the context.
stpred :: TyVarBndr -> Pred
stpred v = 
  let nm n = mkName $ "SeriType" ++ if n == 0 then "" else show n
  in ClassP (nm (tvarkind v)) [VarT (tyvarname v)]

-- Turn a type t into (Typed Exp t)
texpify :: Type -> Type
texpify t = AppT (AppT (ConT ''S.Typed) (ConT ''SIR.Exp)) t

-- Given the haskell type corresponding to an expression, return the type
-- of the haskell function representing a seri value of that type.
--
-- For example
--  input: (Eq a) => a -> Integer
--  output: (Eq a, SeriType a) => Typed Exp (a -> Integer) 
valuetype :: Type -> Type
valuetype ty = 
  let upcon :: Pred -> Pred
      upcon (ClassP n ts) = ClassP (classname n) ts
  in case (flattenforall ty) of
        ForallT vns ctx t -> ForallT vns ((map upcon ctx) ++ (map stpred vns)) (texpify t)
        t -> texpify t 

-- Given the raw haskell type corresponding to an expression, return the type
-- of the haskell function representing the InstId of that expression.
--
-- For example
--  input: (Eq a) => a -> Integer
--  output: Typed Exp (a -> Integer) -> InstId
instidtype :: Type -> Type 
instidtype (ForallT vns _ t) = ForallT vns [] (instidtype t)
instidtype t = arrowts [texpify t, ConT ''SIR.InstId]

-- Given a potentially polymorphic haskell type, convert it to a concrete
-- haskell type which represents the polymorphic seri type.
--
-- In other words, remove all ForallTs and replace all occurences of VarT
-- "foo" with VarT_foo.
concrete :: Type -> Type
concrete = concrete' []

-- concrete' - same as concrete, but lets you leave a list of variable types
-- unconcrete.
concrete' :: [Name] -> Type -> Type
concrete' ns (ForallT _ _ t) = concrete' ns t
concrete' ns t@(VarT nm) | nm `elem` ns = t
concrete' ns (VarT nm) = ConT $ prefixed "VarT_" nm
concrete' ns (AppT a b) = AppT (concrete' ns a) (concrete' ns b)
concrete' ns t = t

-- Given a type, return an expression corresonding to the seri type of
-- that type.
seritypeexp :: Type -> Exp
seritypeexp (VarT nm) = applyC 'SIR.VarT [string nm]
seritypeexp (ForallT vars preds t) =
 let vars' = ListE $ map (string . tyvarname) vars

     mkpred :: Pred -> Exp
     mkpred (ClassP n [t]) = applyC 'SIR.Pred [string n, ListE [seritypeexp t]]

     preds' = ListE $ map mkpred preds
 in applyC 'SIR.ForallT [vars', preds', seritypeexp t]
seritypeexp (ConT nm) = VarE (tycontypename nm)
seritypeexp t = apply 'S.seritype [SigE (VarE 'undefined) (concrete t)]
    
