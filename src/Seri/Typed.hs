
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


module Seri.Typed 
    (
        Typed(..), typedas, TEnv,
        SeriType(..), SeriType1(..), SeriType2(..), SeriType3(..),
        VarT_a(..), VarT_b(..), VarT_c(..), VarT_d(..), VarT_m(..),
    
        integerE, ifE, caseE, conE, varE, dvarE, rdvarE, lamE, appE,
        primitive, match, selector, lamM,
        conP, appP, wildP, integerP,
        valD,
    )
    where

import qualified Language.Haskell.TH as TH

import Seri.IR
import Seri.Env

data Typed x t = Typed {
    typed :: x
}

type TEnv x a = Typed (Env x) a

typedas :: Typed a t -> Typed b t -> Typed b t
typedas _ x = x

class SeriType a where
    seritype :: a -> Type

class SeriType1 m where
    seritype1 :: m a -> Type

instance (SeriType1 m, SeriType a) => SeriType (m a) where  
    seritype m =
        let ta :: m a -> a
            ta _ = undefined
        in AppT (seritype1 m) (seritype (ta m))

class SeriType2 m where
    seritype2 :: m a b -> Type

instance SeriType2 (->) where
    seritype2 _ = ArrowT

instance (SeriType2 m, SeriType a) => SeriType1 (m a) where
    seritype1 ma =
        let ta :: m a b -> a
            ta _ = undefined
        in AppT (seritype2 ma) (seritype (ta ma))

class SeriType3 m where
    seritype3 :: m a b c -> Type
    
instance (SeriType3 m, SeriType a) => SeriType2 (m a) where
    seritype2 ma = 
        let ta :: m a b c -> a
            ta _ = undefined
        in AppT (seritype3 ma) (seritype (ta ma))

-- Dummy haskell types corresponding to variable types in seri.
-- Lets us express polymorphic seri expressions with a concrete haskell type.
data VarT_a = VarT_a
data VarT_b = VarT_b
data VarT_c = VarT_c
data VarT_d = VarT_d

-- And some of kind * -> *.
data VarT_m a = VarT_m

instance SeriType VarT_a where
    seritype _ = VarT "a"

instance SeriType VarT_b where
    seritype _ = VarT "b"

instance SeriType VarT_c where
    seritype _ = VarT "c"

instance SeriType VarT_d where
    seritype _ = VarT "d"

instance SeriType1 VarT_m where
    seritype1 _ = VarT "m"

instance Monad VarT_m where
    return = error $ "return VarT_m"
    (>>=) = error $ ">>= VarT_m"

usetype :: (SeriType a) => Typed x a -> (Type -> b) -> b
usetype e f = f (seritype (gettype e))
    where gettype :: Typed x a -> a
          gettype _ = undefined

-- withtype f 
--  Calls the function f with the Type corresponding to the type of the
--  returned expression.
withtype :: (SeriType a) => (Type -> Typed x a) -> Typed x a
withtype f = r where r = usetype r f

primitive :: (SeriType a) => Name -> TEnv Exp a
primitive p = withtype $ \t -> Typed $ return (PrimE t p)

integerE :: Integer -> TEnv Exp Integer
integerE x = Typed $ return (IntegerE x)

ifE :: (SeriType a) => TEnv Exp Bool -> TEnv Exp a -> TEnv Exp a -> TEnv Exp a
ifE (Typed p) (Typed a) (Typed b)
    = withtype $ \t -> Typed $ do
        p' <- p
        a' <- a
        b' <- b
        return $ IfE t p' a' b'

caseE :: (SeriType b) => TEnv Exp a -> [TEnv Match (a -> b)] -> TEnv Exp b
caseE e matches
  = withtype $ \t -> Typed $ do
        e' <- typed e
        ms <- mapM typed matches
        return $ CaseE t e' ms

match :: Typed Pat a -> TEnv Exp b -> TEnv Match (a -> b)
match p e = Typed $ do
    e' <- typed e
    return $ Match (typed p) e'

conP :: Name -> Typed Pat a
conP n = Typed $ ConP n

appP :: Typed Pat (a -> b) -> Typed Pat a -> Typed Pat b
appP (Typed f) (Typed x) = Typed $ AppP f x

wildP :: Typed Pat a
wildP = Typed WildP

integerP :: Integer -> Typed Pat Integer
integerP i = Typed $ IntegerP i

appE :: (SeriType b) => TEnv Exp (a -> b) -> TEnv Exp a -> TEnv Exp b
appE f x = withtype $ \t -> Typed $ do
    f' <- typed f
    x' <- typed x
    return $ AppE t f' x'

lamE :: (SeriType a, SeriType (a -> b)) => Name -> (TEnv Exp a -> TEnv Exp b) -> TEnv Exp (a -> b)
lamE n f = withtype $ \t -> Typed $ do
    body <- typed $ f (varE n)
    return $ LamE t n body

-- selector dt i fields
-- Create a selector function for ith field of data type dt which has 'fields'
-- number of fields
selector :: (SeriType a, SeriType b) => Name -> Integer -> Integer -> TEnv Exp (a -> b)
selector dt i fields
 = lamE "d" (\d -> caseE d [lamM "x" (\xp xe ->
        let wilds :: Integer -> Typed Pat c -> Typed Pat d
            wilds n (Typed p) = Typed $ foldl AppP p (replicate (fromInteger n) WildP)
            pat = wilds (fields - i - 1) (appP (wilds i (conP dt)) xp)
        in match pat xe
        )])
        

lamM :: (SeriType a) => Name -> (Typed Pat a -> TEnv Exp a -> TEnv Match b) -> TEnv Match b
lamM n f = f (Typed $ VarP n) (varE n)

varE :: (SeriType a) => Name -> TEnv Exp a
varE nm = withtype $ \t -> Typed . return $ VarE t nm

dvarE :: (SeriType a) => TEnv Exp a -> Name -> TEnv Exp a
dvarE (Typed e) nm = withtype $ \t -> Typed $ withenv e (VarE t nm)

rdvarE :: (SeriType a) => TEnv Exp a -> Name -> TEnv Exp a
rdvarE (Typed e) nm = withtype $ \t -> Typed . return $ VarE t nm

conE :: (SeriType a) => Name -> TEnv Exp a
conE nm = withtype $ \t -> Typed . return $ ConE t nm

valD :: (SeriType a) => Name -> TEnv Exp a -> TEnv Exp a
valD nm (Typed e) = withtype $ \t -> Typed $ do
    putenv (ValD nm t (val e)) e

