
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fprof-auto-top #-}

-- | Implementation of Smten primitive Char# type.
module Smten.Runtime.Char (
    Char#(Char#),
    eqChar#, neChar#, gtChar#, geChar#, ltChar#, leChar#,
    ord#, chr#,
    toHSChar#, char#, __isLitChar#,
    primCharApp,
  ) where

import qualified Prelude as P
import qualified GHC.Prim as P

import Smten.Runtime.Bool
import Smten.Runtime.SmtenHS
import Smten.Runtime.Int

-- Note: Code generation currently generates pattern matches of the form:
--    Char# {}
-- Meaning there must be a Char# data constructor. The fields of that
-- constructor don't matter.
--
-- TODO: Wouldn't it be better if the code generator didn't assume this?
newtype Char# = Char# Int#

char# :: P.Char# -> Char#
char# x = Char# (int# (P.ord# x))

__isLitChar# :: P.Char# -> Char# -> Bool
__isLitChar# v (Char# i) = __isLitInt# (P.ord# v) i

toHSChar# :: Char# -> P.Char#
toHSChar# (Char# x) = P.chr# (toHSInt# x)

instance SmtenHS0 Char# where
    ite0 p (Char# a) (Char# b) = Char# (ite0 p a b)
    unreachable0 = Char# unreachable0

eqChar# :: Char# -> Char# -> Bool
eqChar# (Char# a) (Char# b) = a ==# b

neChar# :: Char# -> Char# -> Bool
neChar# (Char# a) (Char# b) = a /=# b

gtChar# :: Char# -> Char# -> Bool
gtChar# (Char# a) (Char# b) = a ># b

geChar# :: Char# -> Char# -> Bool
geChar# (Char# a) (Char# b) = a >=# b

ltChar# :: Char# -> Char# -> Bool
ltChar# (Char# a) (Char# b) = a <# b

leChar# :: Char# -> Char# -> Bool
leChar# (Char# a) (Char# b) = a <=# b

ord# :: Char# -> Int#
ord# (Char# i) = i

chr# :: Int# -> Char#
chr# i = Char# i

primCharApp :: (SmtenHS0 a) => (P.Char# -> a) -> Char# -> a
primCharApp f (Char# x) =
  let g i = f (P.chr# i)
  in primIntApp g x
      

