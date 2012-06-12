
module Seri.Lambda.Typeof (Typeof(..), assign, assignments) where

import Data.Generics

import Seri.Lambda.IR
import Seri.Lambda.Utils

class Typeof a where
    -- Return the seri type of the given object, assuming the object is well
    -- typed. Behavior is undefined it the object is not well typed.
    typeof :: a -> Type

instance Typeof Exp where
    typeof (IntegerE _) = tinteger
    typeof (PrimE tn) = typeof tn
    typeof (CaseE _ (m:_)) = typeof m
    typeof (AppE f _) = outputT (typeof f)
    typeof (LamE tn e) = arrowsT [typeof tn, typeof e]
    typeof (ConE tn) = typeof tn
    typeof (VarE tn _) = typeof tn
    
instance Typeof Sig where
    typeof (Sig _ t) = t

instance Typeof Match where
    typeof (Match _ e) = typeof e

instance Typeof Pat where
    typeof (ConP tn _) = outputT (typeof tn)
    typeof (VarP tn) = typeof tn
    typeof (IntegerP _) = tinteger
    typeof (WildP t) = t

-- The intger type
tinteger :: Type
tinteger = ConT "Integer"

-- assignments poly concrete
-- Given a polymorphic type and a concrete type of the same form, return the
-- mapping from type variable name to concrete type. Assignments are returned
-- in order of how they are listed in the top level forall type, if any.
assignments :: Type -> Type -> [(Name, Type)]
assignments (VarT n) t = [(n, t)]
assignments (AppT a b) (AppT a' b') = (assignments a a') ++ (assignments b b')
assignments (ForallT vars _ t) t' =
  let assigns = assignments t t'
      
      look :: Name -> (Name, Type)
      look n = case lookup n assigns of
                 Just t -> (n, t)
                 Nothing -> error $ "assignment missing for var " ++ n
  in map look vars
assignments _ _ = []

-- assign vs x
--  Replace each occurence of a variable type according to the given mapping
--  in x.
assign :: (Data a) => [(Name, Type)] -> a -> a
assign m =
  let base :: Type -> Type
      base t@(VarT n) = 
        case lookup n m of
            Just t' -> t'
            Nothing -> t
      base t = t
  in everywhere $ mkT base

