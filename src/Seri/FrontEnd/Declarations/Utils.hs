
{-# LANGUAGE TemplateHaskell #-}

module Seri.FrontEnd.Declarations.Utils (
    tvarkind, stpred, valuetype, instidtype, texpify, seritypeexp,
    ) where

import Language.Haskell.TH

import Seri.Utils.TH
import qualified Seri.FrontEnd.Typed as S
import qualified Seri.IR as S
import Seri.FrontEnd.Declarations.Names
import Seri.FrontEnd.Declarations.Polymorphic

-- Given a type variable, figure out what predicate we should add for it
-- in the context.
stpred :: TyVarBndr -> Pred
stpred v = 
  let nm n = mkName $ "SeriType" ++ if n == 0 then "" else show n
  in ClassP (nm (tvarkind v)) [VarT (tyvarname v)]

-- Turn a type t into (Typed Exp t)
texpify :: Type -> Type
texpify t = AppT (AppT (ConT ''S.Typed) (ConT ''S.Exp)) t

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
-- of the haskell function representing the VarInfo of that expression.
--
-- For example
--  input: (Eq a) => a -> Integer
--  output: Typed Exp (a -> Integer) -> VarInfo
instidtype :: Type -> Type 
instidtype (ForallT vns _ t) = ForallT vns [] (instidtype t)
instidtype t = arrowts [texpify t, ConT ''S.VarInfo]

-- Given a type, return an expression corresonding to the seri type of
-- that type.
seritypeexp :: Type -> Exp
seritypeexp (VarT nm) = applyC 'S.VarT [string nm]
seritypeexp (ForallT vars preds t) =
 let vars' = ListE $ map (string . tyvarname) vars

     mkpred :: Pred -> Exp
     mkpred (ClassP n ts) =
        applyC 'S.Pred [string n, ListE $ map seritypeexp ts]

     preds' = ListE $ map mkpred preds
 in applyC 'S.ForallT [vars', preds', seritypeexp t]
seritypeexp (ConT nm) = VarE (tycontypename nm)
seritypeexp t = apply 'S.seritype [SigE (VarE 'undefined) (concrete t)]
    
