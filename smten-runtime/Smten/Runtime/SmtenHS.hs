
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Smten.Runtime.SmtenHS where

import Prelude hiding (Bool(..), Integer)
import qualified Prelude as Prelude
import qualified Prelude as P
import qualified Smten.Bit as P

import System.IO.Unsafe
import System.Mem.StableName

import Data.Bits
import Data.Dynamic
import Data.Maybe(fromMaybe, isJust)

import qualified Smten.AnyMap as A
import Smten.SMT.FreeID
import Smten.CodeGen.TH

data ErrorString =
   ErrorString String
 | ErrorString_Ite Bool ErrorString ErrorString

errstr :: String -> ErrorString
errstr = ErrorString

doerr :: ErrorString -> a
doerr (ErrorString msg) = error $ "smten user error: " ++ msg

data Bool where
    False :: Bool
    True :: Bool
    Bool_Var :: FreeID -> Bool
    Bool_EqInteger :: Integer -> Integer -> Bool
    Bool_LeqInteger :: Integer -> Integer -> Bool
    Bool_EqBit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
    Bool_LeqBit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
    Bool_Ite :: Bool -> Bool -> Bool -> Bool
    Bool_Prim :: (Assignment -> Bool) -> Bool -> Bool
    Bool_Error :: ErrorString -> Bool

instance Show Bool where
    show False = "False"
    show True = "True"
    show (Bool_Var x) = freenm x
    show (Bool_EqInteger {}) = "EqInteger"
    show (Bool_LeqInteger {}) = "LeqInteger"
    show (Bool_EqBit {}) = "EqBit"
    show (Bool_LeqBit {}) = "LeqBit"
    show (Bool_Ite p a b) = "Ite (" ++ show p ++ ") (" ++ show a ++ ") (" ++ show b ++ ")"
    show (Bool_Prim {}) = "Prim"
    show (Bool_Error {}) = "Error"

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | Integer_Ite Bool Integer Integer
  | Integer_Var FreeID
  | Integer_Prim (Assignment -> Integer) Integer
  | Integer_Error ErrorString

data Bit n where
    Bit :: P.Bit -> Bit n
    Bit_Add :: Bit n -> Bit n -> Bit n
    Bit_Sub :: Bit n -> Bit n -> Bit n
    Bit_Mul :: Bit n -> Bit n -> Bit n
    Bit_Or :: Bit n -> Bit n -> Bit n
    Bit_And :: Bit n -> Bit n -> Bit n
    Bit_Shl :: Bit n -> Bit n -> Bit n
    Bit_Lshr :: Bit n -> Bit n -> Bit n
    Bit_Concat :: (SmtenHS0 a, SmtenHS0 b) => Bit a -> Bit b -> Bit n
    Bit_Extract :: (SmtenHS0 a) => Bit a -> Integer -> Bit n
    Bit_Not :: Bit n -> Bit n
    Bit_SignExtend :: (SmtenHS0 m) => Bit m -> Bit n
    Bit_Ite :: Bool -> Bit n -> Bit n -> Bit n
    Bit_Var :: FreeID -> Bit n
    Bit_Prim :: (Assignment -> Bit n) -> Bit n -> Bit n
    Bit_Error :: ErrorString -> Bit n

data Assignment = Assignment {
   as_vars :: [(FreeID, Dynamic)],
   as_cache :: A.AnyMap
}

-- Give a nicer name to 'undefined' to use for specifying the argument to
-- valueof.
numeric :: a
numeric = error "numeric"

as_lookup :: (Typeable a) => FreeID -> Assignment -> a
as_lookup x m = fromMaybe (error "as_lookup failed") $ do
    v <- lookup x (as_vars m)
    fromDynamic v

as_make :: [(FreeID, Dynamic)] -> IO Assignment
as_make vars = do
    cache <- A.new
    return (Assignment vars cache)
  
realize :: (SmtenHS0 a) => Assignment -> a -> a
realize m x = unsafeDupablePerformIO $ do
    let mc = as_cache m
    xfnd <- A.lookup mc x
    case xfnd of
      Just v -> return v
      Nothing -> do
        let v = realize0 m x
        A.insert mc x v
        return v

stableNameEq :: a -> a -> P.Bool
stableNameEq x y = unsafeDupablePerformIO $ do
    xnm <- makeStableName x
    ynm <- makeStableName y
    return (xnm == ynm)

ite :: (SmtenHS0 a) => Bool -> a -> a -> a
ite p a b
 | stableNameEq a b = a
 | otherwise = ite0 p a b

sapp :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> a -> b
sapp = sapp0

class SmtenHS0 a where
    -- Update all variables in the given expression according to the given map.
    realize0 :: Assignment -> a -> a

    error0 :: ErrorString -> a

    -- primitive0 r c
    --   r - Given an assignment, returns the concrete value of the object.
    --   c - The SMT formula for the argument.
    primitive0 :: (Assignment -> a) -> a -> a

    ite0 :: Bool -> a -> a -> a

    -- Apply the function to the argument.
    -- * if arg is error, the function is not called and the result is error.
    -- * if arg is WHNF concrete, the function is called to get the result.
    -- * otherwise
    --    for realize: a thunk is created
    --    for formula: function is called for all WHNF concrete cases of the
    --                 argument, and the results are joined.
    -- In other words: the provided function is guarenteed to be called with a
    -- non-error WHNF concrete argument.
    sapp0 :: (SmtenHS0 b) => (a -> b) -> a -> b

    -- For numeric types, this returns the value of the type.
    -- For other types, this is undefined.
    valueof0 :: a -> P.Integer
    valueof0 = error "valueof0 for non-numeric type"

{-# INLINEABLE iterealize #-}
iterealize :: (SmtenHS0 a) => Bool -> a -> a -> Assignment -> a
iterealize p a b m = __caseTrue (realize m p) (realize m a) (realize m b)

-- flmerge
-- Merge two fields of an ite constructor.
{-# INLINEABLE flmerge #-}
flmerge :: (SmtenHS0 a) => Bool -> Maybe (Bool, a) -> Maybe (Bool, a) -> Maybe (Bool, a)
flmerge _ Nothing Nothing = Nothing
flmerge p (Just (g, v)) Nothing = Just (ite p g False, v)
flmerge p Nothing (Just (g, v)) = Just (ite p False g, v)
flmerge p (Just (ga, va)) (Just (gb, vb)) = Just (ite p ga gb, ite p va vb)

flrealize :: (SmtenHS0 a) => Assignment -> [Maybe (Bool, a)] -> a
flrealize m (Nothing : xs) = flrealize m xs
flrealize m (Just (p, v) : xs) = __caseTrue (realize m p) (realize m v) (flrealize m xs)
flrealize _ [] = error "flrealize failed"

-- flsapp:
{-# INLINEABLE flsapp #-}
flsapp :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> a -> [Maybe (Bool, a)] -> b
flsapp f x zs =
 let join [Just (_, v)] = f v
     join (Just (p, a):bs) = ite p (f a) (join bs)
     zs' = filter isJust zs
 in primitive0 (\m -> realize m (f (realize m x))) (join zs')

{-# INLINEABLE itesapp1 #-}
itesapp1 :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> Bool -> a -> a -> b
itesapp1 f p a b = primitive0 (\m -> __caseTrue (realize m p) (realize m f a) (realize m f b)) (ite p (sapp f a) (sapp f b))

{-# INLINEABLE primsapp #-}
primsapp :: (SmtenHS0 a, SmtenHS0 b) => (a -> b) -> (Assignment -> a) -> a -> b
primsapp f r c = primitive0 (\m -> realize m (f (r m))) (sapp f c)

class Haskelly h s where
    -- Convert from a haskell object to a smten object.
    frhs :: h -> s

    -- Convert a smten object to a haskell object.
    -- The behavior is undefined if the smten object can't be converted to a
    -- haskell object.
    tohs :: s -> h

instance Haskelly a a where
    frhs = id
    tohs = id

instance (Haskelly ha sa, Haskelly hb sb, SmtenHS0 sa, SmtenHS0 sb)
         => Haskelly (ha -> hb) (sa -> sb) where
    frhs hf = sapp (frhs . hf . tohs)
    tohs sf = tohs . sf . frhs

instance Haskelly (a -> b) (a -> b) where
    frhs = id
    tohs = id

__caseTrue :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseTrue x y n = 
  case x of
    True -> y
    False -> n
    _ -> sapp (\v -> __caseTrue v y n) x
 

__caseFalse :: (SmtenHS0 z) => Bool -> z -> z -> z
__caseFalse x y n =
  case x of
    False -> y
    True -> n
    _ -> sapp (\v -> __caseFalse v y n) x

instance SmtenHS0 ErrorString where
   realize0 m x@(ErrorString str) = x
   realize0 m (ErrorString_Ite p a b) = iterealize p a b m
   primitive0 = error "primitive0 called on ErrorString"
   error0 = id
   sapp0 f x = error0 x
   ite0 = ErrorString_Ite

instance SmtenHS0 Bool where
   realize0 m b@True = b
   realize0 m b@False = b
   realize0 m b@(Bool_Error {}) = b
   realize0 m (Bool_Var x) = frhs (as_lookup x m :: P.Bool)
   realize0 m (Bool_EqInteger a b) = eq_Integer (realize m a) (realize m b)
   realize0 m (Bool_LeqInteger a b) = leq_Integer (realize m a) (realize m b)
   realize0 m (Bool_EqBit a b) = eq_Bit (realize m a) (realize m b)
   realize0 m (Bool_LeqBit a b) = leq_Bit (realize m a) (realize m b)
   realize0 m (Bool_Ite p a b) = iterealize p a b m
   realize0 m (Bool_Prim r _) = r m

   primitive0 = Bool_Prim
   error0 = Bool_Error

   sapp0 f x =
     case x of
        False -> f x
        True -> f x
        Bool_Error msg -> error0 msg
        _ -> ite x (f True) (f False)

   ite0 = Bool_Ite

instance Haskelly Bool Bool where
  frhs = id
  tohs = id

instance Haskelly P.Bool Bool where
  frhs p = if p then True else False

  tohs False = P.False
  tohs True = P.True
  tohs _ = error "Bool tohs failed"


instance SmtenHS0 Integer where
   realize0 m x = 
      case x of
         Integer {} -> x
         Integer_Error {} -> x
         Integer_Add a b -> add_Integer (realize m a) (realize m b)
         Integer_Sub a b -> sub_Integer (realize m a) (realize m b)
         Integer_Ite p a b -> iterealize p a b m
         Integer_Var v -> frhs (as_lookup v m :: P.Integer)
         Integer_Prim r _ -> r m

   sapp0 f x =
      case x of
        Integer {} -> f x
        Integer_Error msg -> error0 msg
        Integer_Ite p a b -> ite p (sapp f a) (sapp f b)
        Integer_Prim r c -> primsapp f r c
        _ -> -- TODO: We should generate the infinite sequence of integers:
             --     0, 1, -1, 2, -2, 3, -3, ...
             -- call the function on each one of those integers,
             -- and then join the results.
             -- For debug purposes, this behavior is currently disabled.
          error "TODO: sapp0 for symbolic Integer"

   primitive0 = Integer_Prim
   error0 = Integer_Error
   ite0 = Integer_Ite

instance Haskelly Integer Integer where
   frhs = id
   tohs = id

instance Haskelly P.Integer Integer where
   frhs = Integer

   tohs (Integer x) = x
   tohs _ = error "tohs.Integer failed"


{-# SPECIALIZE iix :: (P.Integer -> P.Integer -> P.Bool) -> (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool #-}
{-# SPECIALIZE iix :: (P.Integer -> P.Integer -> P.Integer) -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer #-}
iix :: (SmtenHS0 sz, Haskelly hz sz) => (P.Integer -> P.Integer -> hz) -> (Integer -> Integer -> sz) -> Integer -> Integer -> sz
iix hf _ (Integer a) (Integer b) = frhs (hf a b)
iix _ _ (Integer_Error msg) _ = error0 msg
iix _ _ _ (Integer_Error msg) = error0 msg
iix _ sf a b = sf a b

eq_Integer :: Integer -> Integer -> Bool
eq_Integer = iix (==) Bool_EqInteger

leq_Integer :: Integer -> Integer -> Bool
leq_Integer = iix (<=) Bool_LeqInteger

add_Integer :: Integer -> Integer -> Integer
add_Integer = iix (+) Integer_Add

sub_Integer :: Integer -> Integer -> Integer
sub_Integer = iix (-) Integer_Sub


instance Haskelly (Bit n) (Bit n) where
   frhs = id
   tohs = id

instance Haskelly P.Bit (Bit n) where
   frhs = Bit

   tohs (Bit c) = c
   tohs _ = error "tohs.Bit failed"

vvx :: (SmtenHS0 n, SmtenHS0 m, SmtenHS0 sz, Haskelly hz sz) => (P.Bit -> P.Bit -> hz) -> (Bit n -> Bit m -> sz) -> Bit n -> Bit m -> sz
vvx hf _ (Bit a) (Bit b) = frhs (hf a b)
vvx _ _ (Bit_Error msg) _ = error0 msg
vvx _ _ _ (Bit_Error msg) = error0 msg
vvx _ sf a b = sf a b

eq_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
eq_Bit = vvx (==) Bool_EqBit

leq_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bool
leq_Bit = vvx (<=) Bool_LeqBit

add_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
add_Bit = vvx (+) Bit_Add

sub_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
sub_Bit = vvx (-) Bit_Sub

mul_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
mul_Bit = vvx (*) Bit_Mul

or_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
or_Bit = vvx ((.|.)) Bit_Or

and_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
and_Bit = vvx ((.&.)) Bit_And

shl_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
shl_Bit = vvx P.bv_shl Bit_Shl

lshr_Bit :: (SmtenHS0 n) => Bit n -> Bit n -> Bit n
lshr_Bit = vvx P.bv_lshr Bit_Lshr

not_Bit :: (SmtenHS0 n) => Bit n -> Bit n
not_Bit (Bit x) = frhs $ complement x
not_Bit (Bit_Error msg) = error0 msg
not_Bit x = Bit_Not x

sign_extend_Bit :: forall n m . (SmtenHS0 n, SmtenHS0 m) => Bit n -> Bit m
sign_extend_Bit (Bit x) =
  let w = (valueof0 (numeric :: m) - valueof0 (numeric :: n))
  in frhs $ P.bv_sign_extend w x
sign_extend_Bit (Bit_Error msg) = error0 msg
sign_extend_Bit x = Bit_SignExtend x

concat_Bit :: (SmtenHS0 n, SmtenHS0 m, SmtenHS0 npm) => Bit n -> Bit m -> Bit npm
concat_Bit = vvx P.bv_concat Bit_Concat

extract_Bit :: forall n m . (SmtenHS0 n, SmtenHS0 m) => Bit n -> Integer -> Bit m
extract_Bit (Bit x) (Integer i) = 
  let hi = i + valueof0 (numeric :: m) - 1
      lo = i
  in frhs (P.bv_extract hi lo x)
extract_Bit (Bit_Error msg) _ = error0 msg
extract_Bit _ (Integer_Error msg) = error0 msg
extract_Bit b i = Bit_Extract b i

toInteger_Bit :: (SmtenHS0 n) => Bit n -> Integer
toInteger_Bit = frhs P.bv_value

declare_SmtenHS 1
declare_SmtenHS 2
declare_SmtenHS 3
declare_SmtenHS 4

derive_SmtenHS 0
derive_SmtenHS 1
derive_SmtenHS 2
derive_SmtenHS 3

instance SmtenHS2 (->) where
   realize2 m f = \x -> realize m (f (realize m x))
   primitive2 r f = \x -> primitive0 (\m -> r m x) (sapp f x)
   error2 msg = \x -> error0 msg
   sapp2 f x = f x
   ite2 p fa fb = \x -> ite p (fa x) (fb x)

instance SmtenHS1 Bit where
   realize1 m c = 
      case c of
         Bit {} -> c
         Bit_Error {} -> c
         Bit_Add a b -> add_Bit (realize m a) (realize m b)
         Bit_Sub a b -> sub_Bit (realize m a) (realize m b)
         Bit_Mul a b -> mul_Bit (realize m a) (realize m b)
         Bit_Or a b -> or_Bit (realize m a) (realize m b)
         Bit_And a b -> and_Bit (realize m a) (realize m b)
         Bit_Shl a b -> shl_Bit (realize m a) (realize m b)
         Bit_Lshr a b -> lshr_Bit (realize m a) (realize m b)
         Bit_Concat a b -> concat_Bit (realize m a) (realize m b)
         Bit_Extract a b -> extract_Bit (realize m a) (realize m b)
         Bit_Not a -> not_Bit (realize m a)
         Bit_SignExtend a -> sign_extend_Bit (realize m a)
         Bit_Ite p a b -> iterealize p a b m
         Bit_Var x -> frhs (as_lookup x m :: P.Bit)
         Bit_Prim r _ -> r m
    
   sapp1 f x =
     case x of
       Bit {} -> f x
       Bit_Error msg -> error0 msg  
       Bit_Ite p a b -> ite p (sapp f a) (sapp f b)
       Bit_Prim r c -> primsapp f r c
       _ -> -- TODO: We should generate the list of bit vectors:
            --     0, 1, 2, ..., 2^n-1
            -- call the function on each one of those bit vectors,
            -- and then join the results.
            -- For debug purposes, this behavior is currently disabled.
         error "TODO: sapp0 for symbolic bit vector"
   primitive1 = Bit_Prim
   error1 = Bit_Error
   ite1 = Bit_Ite
