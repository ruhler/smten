
{-# LANGUAGE TemplateHaskell #-}

module Seri.Declarations.Utils (
    stpred, valuetype, instidtype, texpify, concrete, seritypeexp,
    ) where

import Language.Haskell.TH

import Seri.THUtils
import qualified Seri.Typed as S
import qualified Seri.IR as SIR
import Seri.Declarations.Names

-- Given a type variable, figure out what predicate we should add for it
-- in the context.
--
-- We assume the following:
--   - PlainTV's starting with 'a', 'b', 'c', or 'd' are of kind *
--   - PlainTV's starting with 'm' are of kind * -> *
stpred :: TyVarBndr -> Pred
stpred (PlainTV v) | head (nameBase v) `elem` "abcd" = ClassP ''S.SeriType [VarT v]
stpred (PlainTV v) | head (nameBase v) `elem` "m" = ClassP ''S.SeriType1 [VarT v]
stpred (KindedTV v StarK) = ClassP ''S.SeriType [VarT v]
stpred (KindedTV v (ArrowK StarK StarK)) = ClassP ''S.SeriType1 [VarT v]
stpred v = error $ "TODO: Seri.Declarations.Utils.stdpred " ++ show v

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
concrete (ForallT _ _ t) = concrete t
concrete (VarT nm) = ConT $ mkName ("VarT_" ++ (nameBase nm))
concrete (AppT a b) = AppT (concrete a) (concrete b)
concrete t = t

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
seritypeexp t = apply 'S.seritype [SigE (VarE 'undefined) (concrete t)]
    
