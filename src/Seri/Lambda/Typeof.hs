
module Seri.Lambda.Typeof (Typeof(..)) where

import Seri.Lambda.IR

class Typeof a where
    -- Return the seri type of the given object, assuming the object is well
    -- typed. Behavior is undefined it the object is not well typed.
    typeof :: a -> Type

instance Typeof Exp where
    typeof (IntegerE _) = tinteger
    typeof (PrimE tn) = typeof tn
    typeof (CaseE _ (m:_)) = typeof m
    typeof (AppE f _) = outputtype (typeof f)
    typeof (LamE tn e) = tarrow (typeof tn)) (typeof e)
    typeof (ConE tn) = typeof tn
    typeof (VarE tn _) = typeof tn
    
instance Typeof Sig where
    typeof (Sig _ t) = t

instance Typeof Match where
    typeof (Match _ e) = typeof e

instance Typeof Pat where
    typeof (ConP tn) = typeof tn
    typeof (VarP tn) = typeof tn
    typeof (IntegerP _) = tinteger
    typeof (AppP f _) = outputtype (typeof f)
    typeof (WildP t) = t

-- Return the output type of a function type.
outputtype :: Type -> Type
outputtype (AppT (AppT (ConT "->") _) t) = t

-- given types a and b, form type (a -> b)
tarrow :: Type -> Type -> Type
tarrow a b = AppT (AppT (ConT "->") a) b

-- The intger type
tinteger :: Type
tinteger = ConT "Integer"

