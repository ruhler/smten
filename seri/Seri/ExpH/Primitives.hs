
{-# LANGUAGE PatternGuards #-}

module Seri.ExpH.Primitives(
    notEH, andEH, orEH,
    __prim_eq_IntegerEH, __prim_eq_CharEH,
    __prim_add_IntegerEH, __prim_sub_IntegerEH, __prim_mul_IntegerEH,
    __prim_lt_IntegerEH, __prim_leq_IntegerEH, __prim_gt_IntegerEH,
    __prim_show_IntegerEH,
    __prim_return_IOEH, __prim_bind_IOEH, __prim_nobind_IOEH, __prim_fail_IOEH,
    putCharEH, getContentsEH,
    numericEH, valueofEH,
    ) where

import Data.Functor((<$>))

import Seri.Name
import Seri.Sig
import Seri.Type
import Seri.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar
import Seri.ExpH.Sugar2
import Seri.ExpH.SeriEH
import Seri.ExpH.SeriEHs

unary :: (SeriEH a, SeriEH b) => String -> (a -> b) -> ExpH -> ExpH
unary n f a 
 | Just av <- de_seriEH a = seriEH (f av)
 | otherwise = appEH (varEH (Sig (name n) (seriT f))) a

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

__prim_show_IntegerEH :: ExpH -> ExpH
__prim_show_IntegerEH =
  let f :: Integer -> String
      f = show
  in unary "Prelude.__prim_show_Integer" f

__prim_return_IOEH :: ExpH -> ExpH
__prim_return_IOEH a = ioEH (return a)

__prim_bind_IOEH :: ExpH -> ExpH -> ExpH
__prim_bind_IOEH x f = ioEH $ do
    let Just xio = de_ioEH x
    r <- xio
    let Just fio = de_ioEH (appEH f r)
    fio

__prim_nobind_IOEH :: ExpH -> ExpH -> ExpH
__prim_nobind_IOEH a b
 | Just aio <- de_ioEH a
 , Just bio <- de_ioEH b = ioEH $ aio >> bio

__prim_fail_IOEH :: ExpH -> ExpH
__prim_fail_IOEH a
 | Just v <- de_stringEH a = ioEH $ fail v

putCharEH :: ExpH -> ExpH
putCharEH a
 | Just v <- de_charEH a = ioEH $ putChar v >> return unitEH

getContentsEH :: ExpH
getContentsEH = ioEH $ stringEH <$> getContents

notEH :: ExpH -> ExpH
notEH = unary "not" not

andEH :: ExpH -> ExpH
andEH a
 | Just av <- de_boolEH a
   = lamEH (Sig (name "b") boolT) (if av then id else const falseEH)
 | otherwise
   = appEH (varEH (Sig (name "&&") (arrowsT [boolT, boolT, boolT]))) a

orEH :: ExpH -> ExpH
orEH a
 | Just av <- de_boolEH a
   = lamEH (Sig (name "b") boolT) (if av then const trueEH else id)
 | otherwise
   = appEH (varEH (Sig (name "||") (arrowsT [boolT, boolT, boolT]))) a

numericEH :: Type -> ExpH
numericEH (NumT nt) = conEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))
numericEH t = error $ "numericEH got type: " ++ pretty t

valueofEH :: ExpH -> ExpH
valueofEH x = 
  let NumT nt = typeof x
  in integerEH (nteval nt)

