
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}

module Smten.Runtime.Types (
    Type(..), Any(..),
    ErrorString(..), errstr, doerr,
    Model, model, m_cached, lookupBool, lookupInteger, lookupBit,
    Bool(..), andB, notB, iteB,
    Integer(..), eq_Integer, leq_Integer, add_Integer, sub_Integer,
    Bit(..), eq_Bit, leq_Bit, add_Bit, sub_Bit, mul_Bit,
    or_Bit, and_Bit, shl_Bit, lshr_Bit, not_Bit, concat_Bit,
    sign_extend_Bit, extract_Bit,
    ) where

import Prelude hiding (Bool(..), Integer(..))
import qualified Prelude as P
import qualified Smten.Runtime.Bit as P

import Data.Bits
import GHC.TypeLits

import System.IO.Unsafe
import qualified Smten.Runtime.AnyMap as A
import Smten.Runtime.FreeID

data Type = BoolT | IntegerT | BitT P.Integer
    deriving (Show)

data Any = BoolA Bool
         | IntegerA Integer
         | BitA P.Bit

data ErrorString =
   ErrorString String
 | ErrorString_Ite Bool ErrorString ErrorString
    deriving (Show)

errstr :: String -> ErrorString
errstr = ErrorString

doerr :: ErrorString -> a
doerr (ErrorString msg) = error $ "smten user error: " ++ msg


data Model = Model {
    m_vars :: [(FreeID, Any)],
    m_cache :: A.AnyMap
}

model :: [(FreeID, Any)] -> IO Model
model vars = do
   cache <- A.new
   return (Model vars cache)

-- lookup the value of an object under the given model.
-- The lookup is memoized.
m_cached :: Model -> (Model -> a -> b) -> a -> b
m_cached m f x = unsafeDupablePerformIO $ do
    let mc = m_cache m
    xfnd <- A.lookup mc x
    case xfnd of
       Just v -> return v
       Nothing -> do
         let v = f m x
         A.insert mc x v
         return v

lookupBool :: Model -> FreeID -> Bool
lookupBool m nm
  | Just (BoolA x) <- lookup nm (m_vars m) = x
  | otherwise = error "lookupBool failed"

lookupInteger :: Model -> FreeID -> Integer
lookupInteger m nm
  | Just (IntegerA x) <- lookup nm (m_vars m) = x
  | otherwise = error "lookupInteger failed"

lookupBit :: Model -> FreeID -> Bit n
lookupBit m nm
  | Just (BitA x) <- lookup nm (m_vars m) = Bit x
  | otherwise = error "lookupBit failed"

data Bool where
   True :: Bool
   False :: Bool
   Bool_Ite :: Bool -> Bool -> Bool -> Bool
   Bool_And :: Bool -> Bool -> Bool
   Bool_Not :: Bool -> Bool
   Bool_EqInteger :: Integer -> Integer -> Bool
   Bool_LeqInteger :: Integer -> Integer -> Bool
   Bool_EqBit :: Bit n -> Bit n -> Bool
   Bool_LeqBit :: Bit n -> Bit n -> Bool
   Bool_Var :: FreeID -> Bool
   Bool_Err :: ErrorString -> Bool
   Bool_Prim :: (Model -> Bool) -> Bool -> Bool

instance Show Bool where
    show True = "True"
    show False = "False"
    show (Bool_Ite p a b) = show p ++ " ? " ++ show a ++ " : " ++ show b
    show (Bool_And a b) = show a ++ " & " ++ show b
    show (Bool_Not a) = "~ " ++ show a
    show (Bool_EqInteger a b) = "?EqInteger?"
    show (Bool_LeqInteger a b) = "?LeqInteger?"
    show (Bool_EqBit a b) = "?EqBit?"
    show (Bool_LeqBit a b) = "?LeqBit?"
    show (Bool_Var a) = freenm a
    show (Bool_Err msg) = "Bool_Err " ++ show msg
    show (Bool_Prim r x) = "?Bool_Prim?"

andB :: Bool -> Bool -> Bool
andB True x = x
andB False x = False
andB a True = a
andB a False = False
andB a@(Bool_Err {}) _ = a
andB _ b@(Bool_Err {}) = b
andB a b = Bool_And a b

notB :: Bool -> Bool
notB True = False
notB False = True
notB (Bool_Not x) = x
notB x@(Bool_Err {}) = x
notB x = Bool_Not x

iteB :: Bool -> Bool -> Bool -> Bool
iteB True x _ = x
iteB False _ x = x
iteB (Bool_Not x) a b = iteB x b a
iteB x@(Bool_Err {}) _ _ = x
iteB p True False = p
iteB p False True = notB p
iteB p a b = Bool_Ite p a b

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | Integer_Ite Bool Integer Integer
  | Integer_Var FreeID
  | Integer_Prim (Model -> Integer) Integer
  | Integer_Err ErrorString

eq_Integer :: Integer -> Integer -> Bool
eq_Integer (Integer a) (Integer b) = if a == b then True else False
eq_Integer (Integer_Err msg) _ = Bool_Err msg
eq_Integer _ (Integer_Err msg) = Bool_Err msg
eq_Integer a b = Bool_EqInteger a b

leq_Integer :: Integer -> Integer -> Bool
leq_Integer (Integer a) (Integer b) = if a <= b then True else False
leq_Integer (Integer_Err msg) _ = Bool_Err msg
leq_Integer _ (Integer_Err msg) = Bool_Err msg
leq_Integer a b = Bool_LeqInteger a b

add_Integer :: Integer -> Integer -> Integer
add_Integer (Integer a) (Integer b) = Integer (a + b)
add_Integer (Integer_Err msg) _ = Integer_Err msg
add_Integer _ (Integer_Err msg) = Integer_Err msg
add_Integer a b = Integer_Add a b

sub_Integer :: Integer -> Integer -> Integer
sub_Integer (Integer a) (Integer b) = Integer (a - b)
sub_Integer (Integer_Err msg) _ = Integer_Err msg
sub_Integer _ (Integer_Err msg) = Integer_Err msg
sub_Integer a b = Integer_Sub a b

data Bit (n :: Nat) where
  Bit :: P.Bit -> Bit n
  Bit_Add :: Bit n -> Bit n -> Bit n
  Bit_Sub :: Bit n -> Bit n -> Bit n
  Bit_Mul :: Bit n -> Bit n -> Bit n
  Bit_Or :: Bit n -> Bit n -> Bit n
  Bit_And :: Bit n -> Bit n -> Bit n
  Bit_Shl :: Bit n -> Bit n -> Bit n
  Bit_Lshr :: Bit n -> Bit n -> Bit n
  Bit_Concat :: Bit a -> Bit b -> Bit n
  Bit_Not :: Bit n -> Bit n

  -- Bit_SignExtend by x
  Bit_SignExtend :: P.Integer -> Bit m -> Bit n

  -- Bit_Extract hi lo x
  Bit_Extract :: P.Integer -> P.Integer -> Bit m -> Bit n
  Bit_Ite :: Bool -> Bit n -> Bit n -> Bit n
  Bit_Var :: FreeID -> Bit n
  Bit_Err :: ErrorString -> Bit n
  Bit_Prim :: (Model -> Bit n) -> Bit n -> Bit n

eq_Bit :: Bit n -> Bit n -> Bool
eq_Bit (Bit a) (Bit b) = if a == b then True else False
eq_Bit (Bit_Err msg) _ = Bool_Err msg
eq_Bit _ (Bit_Err msg) = Bool_Err msg
eq_Bit a b = Bool_EqBit a b

leq_Bit :: Bit n -> Bit n -> Bool
leq_Bit (Bit a) (Bit b) = if a <= b then True else False
leq_Bit (Bit_Err msg) _ = Bool_Err msg
leq_Bit _ (Bit_Err msg) = Bool_Err msg
leq_Bit a b = Bool_LeqBit a b

add_Bit :: Bit n -> Bit n -> Bit n
add_Bit (Bit a) (Bit b) = Bit (a + b)
add_Bit (Bit_Err msg) _ = Bit_Err msg
add_Bit _ (Bit_Err msg) = Bit_Err msg
add_Bit a b = Bit_Add a b

sub_Bit :: Bit n -> Bit n -> Bit n
sub_Bit (Bit a) (Bit b) = Bit (a - b)
sub_Bit (Bit_Err msg) _ = Bit_Err msg
sub_Bit _ (Bit_Err msg) = Bit_Err msg
sub_Bit a b = Bit_Sub a b

mul_Bit :: Bit n -> Bit n -> Bit n
mul_Bit (Bit a) (Bit b) = Bit (a * b)
mul_Bit (Bit_Err msg) _ = Bit_Err msg
mul_Bit _ (Bit_Err msg) = Bit_Err msg
mul_Bit a b = Bit_Mul a b

or_Bit :: Bit n -> Bit n -> Bit n
or_Bit (Bit a) (Bit b) = Bit (a .|. b)
or_Bit (Bit_Err msg) _ = Bit_Err msg
or_Bit _ (Bit_Err msg) = Bit_Err msg
or_Bit a b = Bit_Or a b

and_Bit :: Bit n -> Bit n -> Bit n
and_Bit (Bit a) (Bit b) = Bit (a .&. b)
and_Bit (Bit_Err msg) _ = Bit_Err msg
and_Bit _ (Bit_Err msg) = Bit_Err msg
and_Bit a b = Bit_And a b

shl_Bit :: Bit n -> Bit n -> Bit n
shl_Bit (Bit a) (Bit b) = Bit (a `P.bv_shl` b)
shl_Bit (Bit_Err msg) _ = Bit_Err msg
shl_Bit _ (Bit_Err msg) = Bit_Err msg
shl_Bit a b = Bit_Shl a b

lshr_Bit :: Bit n -> Bit n -> Bit n
lshr_Bit (Bit a) (Bit b) = Bit (a `P.bv_lshr` b)
lshr_Bit (Bit_Err msg) _ = Bit_Err msg
lshr_Bit _ (Bit_Err msg) = Bit_Err msg
lshr_Bit a b = Bit_Lshr a b

concat_Bit :: Bit a -> Bit b -> Bit n
concat_Bit (Bit a) (Bit b) = Bit (a `P.bv_concat` b)
concat_Bit (Bit_Err msg) _ = Bit_Err msg
concat_Bit _ (Bit_Err msg) = Bit_Err msg
concat_Bit a b = Bit_Concat a b

not_Bit :: Bit n -> Bit n
not_Bit (Bit a) = Bit (complement a)
not_Bit (Bit_Err msg) = Bit_Err msg
not_Bit a = Bit_Not a

sign_extend_Bit :: P.Integer -> Bit m -> Bit n
sign_extend_Bit by (Bit a) = Bit (P.bv_sign_extend by a)
sign_extend_Bit _ (Bit_Err msg) = Bit_Err msg
sign_extend_Bit by x = Bit_SignExtend by x

extract_Bit :: P.Integer -> P.Integer -> Bit m -> Bit n
extract_Bit hi lo (Bit a) = Bit (P.bv_extract hi lo a)
extract_Bit _ _ (Bit_Err msg) = Bit_Err msg
extract_Bit hi lo x = Bit_Extract hi lo x

