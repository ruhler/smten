
-- | Utilities for working with Seri Types
module Seri.Lambda.Types (
    appsT, arrowsT, outputT, unarrowsT, listT, integerT, unforallT,
    Typeof(..), typeofCon,
    assign, assignments,
    isSubType,
    ) where

import Control.Monad.State
import Data.List(nub)
import Data.Maybe
import Data.Generics

import Seri.Lambda.IR

-- | The Integer type
integerT :: Type
integerT = ConT "Integer"

-- | Given the list of types [a, b, ..., c],
-- Return the applications of those types: (a b ... c)
-- The list must be non-empty.
appsT :: [Type] -> Type
appsT [] = error $ "appsT applied to empty list"
appsT ts = foldl1 AppT ts

-- | Given the list of types [a, b, ..., c]
-- Return the type (a -> b -> ... -> c)
-- The list must be non-empty.
arrowsT :: [Type] -> Type
arrowsT [] = error $ "arrowsT applied to empty list"
arrowsT [t] = t
arrowsT (t:ts) = appsT [ConT "->", t, arrowsT ts]

-- | Given a type of the form (a -> b), returns b.
outputT :: Type -> Type
outputT (AppT (AppT (ConT "->") _) t) = t
outputT t = t

-- | Given a type of the form (a -> b -> ... -> c),
--  returns the list: [a, b, ..., c]
unarrowsT :: Type -> [Type]
unarrowsT (AppT (AppT (ConT "->") a) b) = a : (unarrowsT b)
unarrowsT t = [t]


-- | Given a type a, returns the type [a].
listT :: Type -> Type
listT t = AppT (ConT "[]") t

-- | assignments poly concrete
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

-- | assign vs x
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

class Typeof a where
    -- | Return the seri type of the given object, assuming the object is well
    -- typed. Behavior is undefined if the object is not well typed.
    typeof :: a -> Type

instance Typeof Exp where
    typeof (IntegerE _) = integerT
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
    typeof (ConP tn _) = last $ unarrowsT (typeof tn)
    typeof (VarP tn) = typeof tn
    typeof (IntegerP _) = integerT
    typeof (WildP t) = t

-- | Get the type of a constructor in the given Data declaration.
-- Returns Nothing if there is no such constructor in the declaration.
typeofCon :: Dec -> Name -> Maybe Type
typeofCon (DataD dn vars cons) cn = do
    Con _ ts <- listToMaybe (filter (\(Con n _) -> n == cn) cons)
    let ty = arrowsT (ts ++ [appsT (ConT dn : map VarT vars)])
    if null vars
        then return $ ForallT vars [] ty
        else return ty


-- | isSubType t sub
-- Return True if 'sub' is a concrete type of 't'.
isSubType :: Type -> Type -> Bool
isSubType t sub
  = let isst :: Type -> Type -> State [(Name, Type)] Bool
        isst (ConT n) (ConT n') = return (n == n')
        isst (AppT a b) (AppT a' b') = do
            ar <- isst a a'
            br <- isst b b'
            return (ar && br)
        isst (VarT n) t = do
            modify $ \l -> (n, t) : l
            return True
        isst (ForallT _ _ t) t' = isst t t'
        isst _ _ = return False

        (b, tyvars) = runState (isst t sub) []
        assignnub = nub tyvars
        namenub = nub (map fst assignnub)
     in length namenub == length assignnub && b
    

-- | Extract the body of a ForallT.
unforallT :: Type -> Type
unforallT (ForallT _ _ t) = t
unforallT t = t
