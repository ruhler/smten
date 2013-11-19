

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Compiled.Smten.Smten.Integer (
    Integer(..),
  ) where

import qualified Prelude as P

import Smten.Runtime.Types
import Smten.Runtime.SmtenHS
import Smten.Runtime.SymbolicOf

-- Turn the given symbolic integer into an (infinite) ite tree representing
-- the same integer.
allcases :: Integer -> Integer
allcases y =
  let -- allin l h x
      --    Convert the symbolic integer x into an ite tree assuming 
      --    x >= l and x < h.
      lookin :: P.Integer -> P.Integer -> Integer -> Integer
      lookin l h x
        | l P.== (h P.- 1) = Integer l
        | P.otherwise =
            let m = (l P.+ h) `P.div` 2
            in Integer_Ite (leq_Integer x (Integer (m P.- 1)))
                           (lookin l m x)
                           (lookin m h x)

      -- lookabove l i 
      --    Convert the symbolic integer x into an ite tree assuming
      --    x >= l
      lookabove :: P.Integer -> P.Integer -> Integer -> Integer
      lookabove l i x =
         let h = l P.+ i
         in Integer_Ite (leq_Integer x (Integer h))
                        (lookin l h x)
                        (lookabove h (i P.* 2) x)

      -- lookbelow h i
      --    Convert the symbolic integer x into an ite tree assuming
      --    x < h
      lookbelow :: P.Integer -> P.Integer -> Integer -> Integer
      lookbelow h i x =
         let l = h P.- i
         in Integer_Ite (leq_Integer x (Integer l))
                        (lookbelow l (i P.* 2) x)
                        (lookin l h x)
            
  in Integer_Ite (leq_Integer y (Integer (P.negate 1)))
                       (lookbelow 0 1 y)
                       (lookabove 0 1 y)
        

instance SymbolicOf P.Integer Integer where
    tosym = Integer

    symapp f x =
      case x of
        Integer i -> f i
        Integer_Ite p a b -> ite0 p (f $$ a) (f $$ b)
        _ -> symapp f (allcases x)



instance P.Num Integer where
    fromInteger = Integer
    (+) = P.error "Smten Integer P.Num (+) not supported"
    (*) = P.error "Smten Integer P.Num (*) not supported"
    abs = P.error "Smten Integer P.Num abs not supported"
    signum = P.error "Smten Integer P.Num signum not supported"

