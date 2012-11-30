
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
    __prim_eq_BitEH, __prim_show_BitEH,
    __prim_add_BitEH, __prim_sub_BitEH, __prim_mul_BitEH,
    __prim_and_BitEH, __prim_or_BitEH, __prim_not_BitEH,
    __prim_shl_BitEH, __prim_lshr_BitEH,
    __prim_fromInteger_BitEH, __prim_zeroExtend_BitEH,
    __prim_truncate_BitEH, __prim_extract_BitEH, __prim_concat_BitEH,
    ) where

import Data.Bits
import Data.Functor((<$>))

import Seri.Name
import Seri.Bit
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

__prim_eq_BitEH :: ExpH -> ExpH -> ExpH
__prim_eq_BitEH a b
  | Just av <- de_bitEH a
  , Just bv <- de_bitEH b = boolEH (av == bv)
  | otherwise =
     let t = arrowsT [typeof a, typeof b, boolT]
     in appsEH (varEH (Sig (name "Seri.Bit.__prim_eq_Bit") t)) [a, b]

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

__prim_show_BitEH :: ExpH -> ExpH
__prim_show_BitEH a
  | Just av <- de_bitEH a = stringEH (show av)
  | otherwise = 
      let t = arrowsT [typeof a, stringT]
      in appEH (varEH (Sig (name "Seri.Bit.__prim_show_Bit") t)) a

__prim_fromInteger_BitEH :: Type -> ExpH -> ExpH
__prim_fromInteger_BitEH t a
  | Just v <- de_integerEH a
  , Just (_, bt) <- de_arrowT t
  , Just w <- de_bitT bt = bitEH (bv_make w v)
  | otherwise = appEH (varEH (Sig (name "Seri.Bit.__prim_fromInteger_Bit") t)) a

__prim_zeroExtend_BitEH :: Type -> ExpH -> ExpH
__prim_zeroExtend_BitEH t a
  | Just v <- de_bitEH a =
     let [ta, AppT _ (NumT wt)] = de_arrowsT t
     in bitEH $ bv_zero_extend (nteval wt - bv_width v) v
  | otherwise = appEH (varEH (Sig (name "Seri.Bit.__prim_zeroExtend_Bit") t)) a
  
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
notEH = unary "Prelude.not" not

andEH :: ExpH -> ExpH
andEH a
 | Just av <- de_boolEH a
   = lamEH (Sig (name "b") boolT) (if av then id else const falseEH)
 | otherwise
   = appEH (varEH (Sig (name "Prelude.&&") (arrowsT [boolT, boolT, boolT]))) a

orEH :: ExpH -> ExpH
orEH a
 | Just av <- de_boolEH a
   = lamEH (Sig (name "b") boolT) (if av then const trueEH else id)
 | otherwise
   = appEH (varEH (Sig (name "Prelude.||") (arrowsT [boolT, boolT, boolT]))) a

numericEH :: Type -> ExpH
numericEH (NumT nt) = conEH (Sig (name "#" `nappend` name (show (nteval nt))) (NumT nt))
numericEH t = error $ "numericEH got type: " ++ pretty t

valueofEH :: ExpH -> ExpH
valueofEH x = 
  let NumT nt = typeof x
  in integerEH (nteval nt)

binaryB :: String -> (Bit -> Bit -> Bit) -> ExpH -> ExpH -> ExpH
binaryB n f a b
 | Just av <- de_bitEH a
 , Just bv <- de_bitEH b = bitEH (f av bv)
 | otherwise =
    let t = arrowsT [typeof a, typeof b, typeof a]
    in appsEH (varEH (Sig (name n) t)) [a, b]

__prim_add_BitEH :: ExpH -> ExpH -> ExpH
__prim_add_BitEH = binaryB "Seri.Bit.__prim_add_Bit" (+)

__prim_sub_BitEH :: ExpH -> ExpH -> ExpH
__prim_sub_BitEH = binaryB "Seri.Bit.__prim_sub_Bit" (-)

__prim_mul_BitEH :: ExpH -> ExpH -> ExpH
__prim_mul_BitEH = binaryB "Seri.Bit.__prim_mul_Bit" (*)

__prim_and_BitEH :: ExpH -> ExpH -> ExpH
__prim_and_BitEH = binaryB "Seri.Bit.__prim_and_Bit" (.&.)

__prim_or_BitEH :: ExpH -> ExpH -> ExpH
__prim_or_BitEH = binaryB "Seri.Bit.__prim_or_Bit" (.|.)

__prim_shl_BitEH :: ExpH -> ExpH -> ExpH
__prim_shl_BitEH = binaryB "Seri.Bit.__prim_shl_Bit" bv_shl

__prim_lshr_BitEH :: ExpH -> ExpH -> ExpH
__prim_lshr_BitEH = binaryB "Seri.Bit.__prim_lshr_Bit" bv_lshr

__prim_not_BitEH :: ExpH -> ExpH
__prim_not_BitEH a
 | Just av <- de_bitEH a = bitEH (complement av)
 | otherwise =
    let t = arrowsT [typeof a, typeof a]
    in appEH (varEH (Sig (name "Seri.Bit.__prim_not_Bit") t)) a

__prim_truncate_BitEH :: Type -> ExpH -> ExpH
__prim_truncate_BitEH t a
 | Just v <- de_bitEH a =
    let [ta, AppT _ (NumT wt)] = de_arrowsT t
    in bitEH $ bv_truncate (nteval wt) v
 | otherwise = appEH (varEH (Sig (name "Seri.Bit.__prim_truncate_Bit") t)) a

__prim_extract_BitEH :: Type -> ExpH -> ExpH -> ExpH
__prim_extract_BitEH t a j
 | Just av <- de_bitEH a
 , Just jv <- de_integerEH j =
   let AppT _ (NumT wt) = last $ de_arrowsT t
       i = jv + (nteval wt) - 1
   in bitEH $ bv_extract i jv av
 | otherwise = appsEH (varEH (Sig (name "Seri.Bit.__prim_extract_BitEH") t)) [a, j]


__prim_concat_BitEH :: ExpH -> ExpH -> ExpH
__prim_concat_BitEH a b
 | Just av <- de_bitEH a
 , Just bv <- de_bitEH b = bitEH (bv_concat av bv)
 | otherwise =
    let Just wa = de_bitT $ typeof a
        Just wb = de_bitT $ typeof b
        t = arrowsT [typeof a, typeof b, bitT (wa+wb)]
    in appsEH (varEH (Sig (name "Seri.Bit.__prim_concat_BitEH") t)) [a, b]
    
