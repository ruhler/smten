

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Integer (
    Integer(..),
  ) where

import qualified Prelude as P

import Smten.Runtime.Formula
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

type Integer = IntegerF

-- Turn the given symbolic integer into an (infinite) ite tree representing
-- the same integer.
allcases :: IntegerF -> IntegerF
allcases y =
  let -- allin l h x
      --    Convert the symbolic integer x into an ite tree assuming 
      --    x >= l and x < h.
      lookin :: P.Integer -> P.Integer -> IntegerF -> IntegerF
      lookin l h x
        | l P.== (h P.- 1) = IntegerF l
        | P.otherwise =
            let m = (l P.+ h) `P.div` 2
            in Integer_Ite (leq_Integer x (IntegerF (m P.- 1)))
                           (lookin l m x)
                           (lookin m h x)

      -- lookabove l i 
      --    Convert the symbolic integer x into an ite tree assuming
      --    x >= l
      lookabove :: P.Integer -> P.Integer -> IntegerF -> IntegerF
      lookabove l i x =
         let h = l P.+ i
         in Integer_Ite (leq_Integer x (IntegerF h))
                        (lookin l h x)
                        (lookabove h (i P.* 2) x)

      -- lookbelow h i
      --    Convert the symbolic integer x into an ite tree assuming
      --    x < h
      lookbelow :: P.Integer -> P.Integer -> IntegerF -> IntegerF
      lookbelow h i x =
         let l = h P.- i
         in Integer_Ite (leq_Integer x (IntegerF l))
                        (lookbelow l (i P.* 2) x)
                        (lookin l h x)
            
  in Integer_Ite (leq_Integer y (IntegerF (P.negate 1)))
                       (lookbelow 0 1 y)
                       (lookabove 0 1 y)
        

instance SymbolicOf P.Integer IntegerF where
    tosym = IntegerF

    symapp f x =
      case x of
        IntegerF i -> f i
        Integer_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        _ -> symapp f (allcases x)



instance P.Num IntegerF where
    fromInteger = IntegerF
    (+) = P.error "Smten IntegerF P.Num (+) not supported"
    (*) = P.error "Smten IntegerF P.Num (*) not supported"
    abs = P.error "Smten IntegerF P.Num abs not supported"
    signum = P.error "Smten IntegerF P.Num signum not supported"

