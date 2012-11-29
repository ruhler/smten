
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Primitives(
    __prim_eq_IntegerEH, __prim_eq_CharEH,
    __prim_add_IntegerEH, __prim_sub_IntegerEH, __prim_mul_IntegerEH,
    __prim_lt_IntegerEH, __prim_leq_IntegerEH, __prim_gt_IntegerEH,
    ) where

import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.SeriEH

binary :: (SeriEH a, SeriEH b, SeriEH c)
          => String -> (a -> b -> c) -> ExpH -> ExpH -> ExpH
binary n f a b
 | Just av <- de_seriEH a
 , Just bv <- de_seriEH b = seriEH (f av bv)
 | otherwise = appsEH (varEH (Sig (name n) (seriT f))) [a, b]

__prim_eq_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_eq_IntegerEH =
  let f :: Integer -> Integer -> Bool
      f = (==)
  in binary "Prelude.__prim_eq_Integer" f

__prim_eq_CharEH :: ExpH -> ExpH -> ExpH
__prim_eq_CharEH =
  let f :: Char -> Char -> Bool
      f = (==)
  in binary "Prelude.__prim_eq_Char" f

__prim_add_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_add_IntegerEH =
  let f :: Integer -> Integer -> Integer
      f = (+)
  in binary "Prelude.__prim_add_Integer" f

__prim_sub_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_sub_IntegerEH =
  let f :: Integer -> Integer -> Integer
      f = (-)
  in binary "Prelude.__prim_sub_Integer" f

__prim_mul_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_mul_IntegerEH =
  let f :: Integer -> Integer -> Integer
      f = (*)
  in binary "Prelude.__prim_mul_Integer" f

__prim_lt_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_lt_IntegerEH =
  let f :: Integer -> Integer -> Bool
      f = (<)
  in binary "Prelude.<" f

__prim_leq_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_leq_IntegerEH =
  let f :: Integer -> Integer -> Bool
      f = (<=)
  in binary "Prelude.<=" f

__prim_gt_IntegerEH :: ExpH -> ExpH -> ExpH
__prim_gt_IntegerEH =
  let f :: Integer -> Integer -> Bool
      f = (>)
  in binary "Prelude.>" f

