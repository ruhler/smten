
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}

module Smten.Runtime.Types (
    Type(..), Any(..),
    ErrorString(..), errstr, doerr, stableNameEq,
    Model, model, m_vars, m_cached, lookupBool, lookupInteger, lookupBit,
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
import System.Mem.StableName

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
lookupBool m nm =
  case lookup nm (m_vars m) of
    Just (BoolA x) -> x
    Just _ -> error "lookupBool: type mismatch"
    Nothing -> False    -- any value will do for the default.

lookupInteger :: Model -> FreeID -> Integer
lookupInteger m nm =
  case lookup nm (m_vars m) of
    Just (IntegerA x) -> x
    Just _ -> error "lookupInteger: type mismatch"
    Nothing -> Integer 0

lookupBit :: Model -> P.Integer -> FreeID -> Bit n
lookupBit m w nm =
  case lookup nm (m_vars m) of
    Just (BitA x) -> Bit x
    Just _ -> error "lookupBit: type mismatch"
    Nothing -> Bit (P.bv_make w 0)

data Bool where
   True :: Bool
   False :: Bool
   Bool_Ite :: Bool -> Bool -> Bool -> Bool
   Bool_And :: Bool -> Bool -> Bool
   Bool_Not :: Bool -> Bool
   Bool_EqInteger :: Integer -> Integer -> Bool
   Bool_LeqInteger :: Integer -> Integer -> Bool
   Bool_EqBit :: P.Integer -> Bit n -> Bit n -> Bool
   Bool_LeqBit :: P.Integer -> Bit n -> Bit n -> Bool
   Bool_Var :: FreeID -> Bool
   Bool_Err :: ErrorString -> Bool

instance Show Bool where
    show True = "True"
    show False = "False"
    show (Bool_Ite p a b) = "(" ++ show p ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
    show (Bool_And a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
    show (Bool_Not a) = "~ " ++ show a
    show (Bool_EqInteger a b) = "?EqInteger?"
    show (Bool_LeqInteger a b) = "?LeqInteger?"
    show (Bool_EqBit _ a b) = "(" ++ show a ++ " == " ++ show b ++ ")"
    show (Bool_LeqBit _ a b) = "(" ++ show a ++ " <= " ++ show b ++ ")"
    show (Bool_Var a) = freenm a
    show (Bool_Err msg) = "Bool_Err " ++ show msg

andB :: Bool -> Bool -> Bool
andB True x = x
andB False x = False
andB a@(Bool_Err {}) _ = a
andB a True = a
andB a False = False
--andB _ b@(Bool_Err {}) = b
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
iteB p True True = True
iteB p False b = notB p `andB` b
iteB p a False = p `andB` a
iteB p a b = Bool_Ite p a b

data Integer =
    Integer P.Integer
  | Integer_Add Integer Integer
  | Integer_Sub Integer Integer
  | Integer_Ite Bool Integer Integer
  | Integer_Var FreeID
  | Integer_Err ErrorString

eq_Integer :: Integer -> Integer -> Bool
eq_Integer (Integer a) (Integer b) = if a == b then True else False
eq_Integer a b | a `symeq` b = True
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

  -- Bit_Concat a_width a b 
  Bit_Concat :: P.Integer -> Bit a -> Bit b -> Bit n
  Bit_Not :: Bit n -> Bit n

  -- Bit_SignExtend by x
  Bit_SignExtend :: P.Integer -> Bit m -> Bit n

  -- Bit_Extract x_width hi lo x
  Bit_Extract :: P.Integer -> P.Integer -> P.Integer -> Bit m -> Bit n
  Bit_Ite :: Bool -> Bit n -> Bit n -> Bit n
  Bit_Var :: P.Integer -> FreeID -> Bit n
  Bit_Err :: ErrorString -> Bit n

instance Show (Bit n) where
  show (Bit x) = show x
  show (Bit_Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Bit_Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (Bit_Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (Bit_Or a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (Bit_And a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (Bit_Shl {}) = "?Bit_Shl?"
  show (Bit_Lshr {}) = "?Bit_Lshr?"
  show (Bit_Concat {}) = "?Bit_Concat?"
  show (Bit_Not a) = "~ " ++ show a
  show (Bit_SignExtend {}) = "?Bit_SignExtend?"
  show (Bit_Extract _ hi lo x) = show x ++ "[" ++ show hi ++ ":" ++ show lo ++ "]"
  show (Bit_Ite p a b) = "(" ++ show p ++ " ? " ++ show a ++ " : " ++ show b ++ ")"
  show (Bit_Var _ x) = freenm x
  show (Bit_Err msg) = "Bit_Err " ++ show msg
    

eq_Bit :: P.Integer -> Bit n -> Bit n -> Bool
eq_Bit _ (Bit a) (Bit b) = if a == b then True else False
eq_Bit _ a b | a `symeq` b = True
eq_Bit _ (Bit_Err msg) _ = Bool_Err msg
eq_Bit _ _ (Bit_Err msg) = Bool_Err msg
eq_Bit w a b = Bool_EqBit w a b

leq_Bit :: P.Integer -> Bit n -> Bit n -> Bool
leq_Bit _ (Bit a) (Bit b) = if a <= b then True else False
leq_Bit _ (Bit_Err msg) _ = Bool_Err msg
leq_Bit _ _ (Bit_Err msg) = Bool_Err msg
leq_Bit w a b = Bool_LeqBit w a b

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

concat_Bit :: P.Integer -> Bit a -> Bit b -> Bit n
concat_Bit _ (Bit a) (Bit b) = Bit (a `P.bv_concat` b)
concat_Bit _ (Bit_Err msg) _ = Bit_Err msg
concat_Bit _ _ (Bit_Err msg) = Bit_Err msg
concat_Bit w a b = Bit_Concat w a b

not_Bit :: Bit n -> Bit n
not_Bit (Bit a) = Bit (complement a)
not_Bit (Bit_Err msg) = Bit_Err msg
not_Bit a = Bit_Not a

sign_extend_Bit :: P.Integer -> Bit m -> Bit n
sign_extend_Bit by (Bit a) = Bit (P.bv_sign_extend by a)
sign_extend_Bit _ (Bit_Err msg) = Bit_Err msg
sign_extend_Bit by x = Bit_SignExtend by x

extract_Bit :: P.Integer -> P.Integer -> P.Integer -> Bit m -> Bit n
extract_Bit _ hi lo (Bit a) = Bit (P.bv_extract hi lo a)
extract_Bit _ _ _ (Bit_Err msg) = Bit_Err msg
extract_Bit wx hi lo x = Bit_Extract wx hi lo x

stableNameEq :: a -> a -> P.Bool
stableNameEq x y = unsafeDupablePerformIO $ do
    xnm <- makeStableName x
    ynm <- makeStableName y
    return (xnm == ynm)

class SymEq a where
    -- Return 'True' if the two (symbolic) objects are structurally equal.
    -- Error constructors are never equal (even if they have the same message).
    symeq :: a -> a -> P.Bool

instance SymEq Bool where
    symeq True True = P.True
    symeq False False = P.True
    symeq a b | a `stableNameEq` b = P.True
    symeq (Bool_Ite a1 a2 a3) (Bool_Ite b1 b2 b3)
        = a1 `symeq` b1 && a2 `symeq` b2 && a3 `symeq` b3
    symeq (Bool_And a1 a2) (Bool_And b1 b2)
        = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bool_Not a) (Bool_Not b) = a `symeq` b
    symeq (Bool_EqInteger a1 a2) (Bool_EqInteger b1 b2)
        = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bool_LeqInteger a1 a2) (Bool_LeqInteger b1 b2)
        = a1 `symeq` b1 && a2 `symeq` b2
--  Note: we can't do these checks, because we don't know statically that the
--  equalities are for the same bitwidth.
--    symeq (Bool_EqBit _ a1 a2) (Bool_EqBit _ b1 b2)
--        = a1 `symeq` b1 && a2 `symeq` b2
--    symeq (Bool_LeqBit _ a1 a2) (Bool_LeqBit _ b1 b2)
--        = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bool_Var a) (Bool_Var b) = a == b
    symeq a b = P.False

instance SymEq Integer where
    symeq (Integer a) (Integer b) = a == b
    symeq a b | a `stableNameEq` b = P.True
    symeq (Integer_Add a1 a2) (Integer_Add b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Integer_Sub a1 a2) (Integer_Sub b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Integer_Ite ap a1 a2) (Integer_Ite bp b1 b2)
      = ap `symeq` bp && a1 `symeq` b1 && a2 `symeq` b2
    symeq (Integer_Var a) (Integer_Var b) = a == b
    symeq a b = P.False
    
instance SymEq (Bit n) where
    symeq (Bit a) (Bit b) = a == b
    symeq a b | a `stableNameEq` b = P.True
    symeq (Bit_Add a1 a2) (Bit_Add b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Sub a1 a2) (Bit_Sub b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Mul a1 a2) (Bit_Mul b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Or a1 a2) (Bit_Or b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_And a1 a2) (Bit_And b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Shl a1 a2) (Bit_Shl b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Lshr a1 a2) (Bit_Lshr b1 b2)
      = a1 `symeq` b1 && a2 `symeq` b2
--    symeq (Bit_Concat _ a1 a2) (Bit_Concat _ b1 b2)
--      = a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Not a) (Bit_Not b) = a `symeq` b
--    symeq (Bit_SignExtend a1 a2) (Bit_SignExtend b1 b2)
--        = a1 == b1 && a2 `symeq` b2
--    symeq (Bit_Extract _ a1 a2 a3) (Bit_Extract _ b1 b2 b3)
--        = a1 == b1 && a2 == b2 && a3 `symeq` b3
    symeq (Bit_Ite ap a1 a2) (Bit_Ite bp b1 b2)
      = ap `symeq` bp && a1 `symeq` b1 && a2 `symeq` b2
    symeq (Bit_Var _ a) (Bit_Var _ b) = a == b
    symeq a b = P.False
