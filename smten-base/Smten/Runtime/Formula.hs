
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds #-}

module Smten.Runtime.Formula (
    BoolF(..), trueF, falseF, boolF, andF, notF, iteF, varF, finiteF,
    isTrueF, isFalseF, (*.),

    IntegerF(..), eq_Integer, leq_Integer, add_Integer, sub_Integer,

    BitF(..), eq_Bit, leq_Bit, add_Bit, sub_Bit, mul_Bit,
    or_Bit, and_Bit, shl_Bit, lshr_Bit, not_Bit, concat_Bit,
    sign_extend_Bit, extract_Bit,
    ) where

import Data.Bits
import GHC.TypeLits

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.FiniteFormula
import qualified Smten.Runtime.Select as S


-- | Representation of a boolean formula which may contain infinite parts or _|_
-- We represent the formula as follows:
--  BoolF a b x_
--    Where:
--     * Logically this is equivalent to:  a + b*x_
--     * Note: b*x_ can only be true if b is satisfiable
--       That is, b is an approximation of b*x_
--     * 'a' and 'b' are finite
--     * 'x_' has not yet finished evaluating to weak head normal form: it
--       might be _|_.
--  By convention in the code that follows, a boolean variable
--  name ending in an underscore refers to a potential _|_ value. A boolean
--  variable name not ending in an underscore refers to a finite value.
data BoolF = BoolF BoolFF BoolFF BoolF
     deriving (Show)

-- Construct a finite BoolF of the form:
--   a
-- where a is finite.
finiteF :: BoolFF -> BoolF
finiteF x = BoolF x falseFF (error "finiteF._|_")

-- Construct a partially finite BoolF of the form:
--   a + bx_
-- where a, b are finite, x_ is potentially _|_
-- This is lazy in x_.
partialF :: BoolFF -> BoolFF -> BoolF -> BoolF
partialF = BoolF

-- Select between two formulas.
-- selectF x_ y_
--   x_, y_ may be infinite.
-- The select call waits for at least one of x_ or y_ to reach weak head
-- normal form, then returns WHNF representations for both x_ and y_.
selectF :: BoolF -> BoolF -> (BoolF, BoolF)
selectF x_ y_ = 
  case S.select x_ y_ of
    S.Both x y -> (x, y)
    S.Left x -> (x, BoolF falseFF trueFF y_)
    S.Right y -> (BoolF falseFF trueFF x_, y)

trueF :: BoolF
trueF = finiteF trueFF

falseF :: BoolF
falseF = finiteF falseFF

boolF :: Bool -> BoolF
boolF True = trueF
boolF False = falseF

varF :: FreeID -> BoolF
varF = finiteF . varFF

-- Notes
--  Partial:    ~(a + bx_)
--            = (~a)(~(bx_))
--            = (~a)(~b + (~x_))
--            = (~a)(~b) + (~a)(~x_)
notF :: BoolF -> BoolF
notF (BoolF a b x_) =
  let nota = (-a)
  in partialF (nota * (-b)) nota (notF x_)

-- x_ * y_
andF :: BoolF -> BoolF -> BoolF
andF x_ y_ =
  case selectF x_ y_ of
    (BoolF xa xb xc_, BoolF ya yb yc_) ->
      let a = xa * ya
          xayb = xa * yb
          yaxb = ya * xb
          b = xayb + yaxb + xb * yb
          c_ = xayb *. yc_ + yaxb *. xc_ + xc_ * yc_
      in partialF a b c_

-- x_ + y_
orF :: BoolF -> BoolF -> BoolF
orF x_ y_ =
  case selectF x_ y_ of
    (BoolF xa xb xc_, BoolF ya yb yc_) ->
      let a = xa + ya
          b = xb + yb
          c_ = xb *. xc_ + yb *. yc_
      in partialF a b c_

iteF :: BoolF -> BoolF -> BoolF -> BoolF
iteF p a b = (p `andF` a) `orF` (notF p `andF` b)

-- For nicer syntax, we give an instance of Num for BoolF
-- based on boolean arithmetic.
instance Num BoolF where
  fromInteger 0 = falseF
  fromInteger 1 = trueF
  (+) = orF
  (*) = andF
  negate = notF
  abs = error "BoolF.abs"
  signum = error "BoolF.signum"

-- | Logical AND of a finite formula and a partial formula.
(*.) :: BoolFF -> BoolF -> BoolF
(*.) x (BoolF a b c_) = BoolF (x*a) (x*b) c_

-- | Return True if this object is equal to trueF
isTrueF :: BoolF -> Bool
isTrueF (BoolF TrueFF FalseFF _) = True
isTrueF _ = False

-- | Return True if this object is equal to falseF
isFalseF :: BoolF -> Bool
isFalseF (BoolF FalseFF FalseFF _) = True
isFalseF _ = False

data IntegerF =
    IntegerF Integer
  | Integer_Add IntegerF IntegerF
  | Integer_Sub IntegerF IntegerF
  | Integer_Ite BoolF IntegerF IntegerF
  | Integer_Var FreeID

eq_Integer :: IntegerF -> IntegerF -> BoolF
eq_Integer (IntegerF a) (IntegerF b) = boolF (a == b)
eq_Integer a b = error "TODO: eq_Integer on symbolic args"

leq_Integer :: IntegerF -> IntegerF -> BoolF
leq_Integer (IntegerF a) (IntegerF b) = boolF (a <= b)
leq_Integer a b = error "TODO: leq_Integer on symbolic args"

add_Integer :: IntegerF -> IntegerF -> IntegerF
add_Integer (IntegerF a) (IntegerF b) = IntegerF (a + b)
add_Integer a b = Integer_Add a b

sub_Integer :: IntegerF -> IntegerF -> IntegerF
sub_Integer (IntegerF a) (IntegerF b) = IntegerF (a - b)
sub_Integer a b = Integer_Sub a b

data BitF (n :: Nat) where
  BitF :: Bit -> BitF n
  Bit_Add :: BitF n -> BitF n -> BitF n
  Bit_Sub :: BitF n -> BitF n -> BitF n
  Bit_Mul :: BitF n -> BitF n -> BitF n
  Bit_Or :: BitF n -> BitF n -> BitF n
  Bit_And :: BitF n -> BitF n -> BitF n
  Bit_Shl :: BitF n -> BitF n -> BitF n
  Bit_Lshr :: BitF n -> BitF n -> BitF n

  -- Bit_Concat a_width a b 
  Bit_Concat :: Integer -> BitF a -> BitF b -> BitF n
  Bit_Not :: BitF n -> BitF n

  -- Bit_SignExtend by x
  Bit_SignExtend :: Integer -> BitF m -> BitF n

  -- Bit_Extract x_width hi lo x
  Bit_Extract :: Integer -> Integer -> Integer -> BitF m -> BitF n
  Bit_Ite :: BoolF -> BitF n -> BitF n -> BitF n
  Bit_Var :: Integer -> FreeID -> BitF n

instance Show (BitF n) where
  show (BitF x) = show x
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
    

eq_Bit :: Integer -> BitF n -> BitF n -> BoolF
eq_Bit _ (BitF a) (BitF b) = boolF (a == b)
eq_Bit w a b = error "TODO: eq_Bit with symbolic args"

leq_Bit :: Integer -> BitF n -> BitF n -> BoolF
leq_Bit _ (BitF a) (BitF b) = boolF (a <= b)
leq_Bit w a b = error "TODO: leq_Bit with symbolic args"

add_Bit :: BitF n -> BitF n -> BitF n
add_Bit (BitF a) (BitF b) = BitF (a + b)
add_Bit a b = Bit_Add a b

sub_Bit :: BitF n -> BitF n -> BitF n
sub_Bit (BitF a) (BitF b) = BitF (a - b)
sub_Bit a b = Bit_Sub a b

mul_Bit :: BitF n -> BitF n -> BitF n
mul_Bit (BitF a) (BitF b) = BitF (a * b)
mul_Bit a b = Bit_Mul a b

or_Bit :: BitF n -> BitF n -> BitF n
or_Bit (BitF a) (BitF b) = BitF (a .|. b)
or_Bit a b = Bit_Or a b

and_Bit :: BitF n -> BitF n -> BitF n
and_Bit (BitF a) (BitF b) = BitF (a .&. b)
and_Bit a b = Bit_And a b

shl_Bit :: BitF n -> BitF n -> BitF n
shl_Bit (BitF a) (BitF b) = BitF (a `bv_shl` b)
shl_Bit a b = Bit_Shl a b

lshr_Bit :: BitF n -> BitF n -> BitF n
lshr_Bit (BitF a) (BitF b) = BitF (a `bv_lshr` b)
lshr_Bit a b = Bit_Lshr a b

concat_Bit :: Integer -> BitF a -> BitF b -> BitF n
concat_Bit _ (BitF a) (BitF b) = BitF (a `bv_concat` b)
concat_Bit w a b = Bit_Concat w a b

not_Bit :: BitF n -> BitF n
not_Bit (BitF a) = BitF (complement a)
not_Bit a = Bit_Not a

sign_extend_Bit :: Integer -> BitF m -> BitF n
sign_extend_Bit by (BitF a) = BitF (bv_sign_extend by a)
sign_extend_Bit by x = Bit_SignExtend by x

extract_Bit :: Integer -> Integer -> Integer -> BitF m -> BitF n
extract_Bit _ hi lo (BitF a) = BitF (bv_extract hi lo a)
extract_Bit wx hi lo x = Bit_Extract wx hi lo x

