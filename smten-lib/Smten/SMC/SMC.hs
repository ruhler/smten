
-- Implementation of symbolic model checking with SMTs.
-- Based on the paper: "Symbolic Model Checking without BDDs"
--  by Armin Biere, Alessandro Cimatti, Edmund Clarke, and Yunshan Zhu
--   January 4, 1999
--   Submitted for TACAS'99
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
{-# LANGUAGE PatternGuards #-}
module Smten.SMC.SMC (Model(..), LTL(..), check) where

import Smten.Prelude hiding (succ)
import Smten.Data.Array
import Smten.Symbolic

data Model s = Model {
    _I :: s -> Bool,        -- | The initial states
    _T :: s -> s -> Bool    -- | The state transition relation
}

data LTL s =
    P (s -> Bool)         -- p
  | (LTL s) :/\ (LTL s)   -- f and g
  | (LTL s) :\/ (LTL s)   -- f or g
  | G (LTL s)             -- G f
  | F (LTL s)             -- F f
  | X (LTL s)             -- X f
  | U (LTL s) (LTL s)     -- f U g
  | R (LTL s) (LTL s)     -- f R g

-- Definition 10 (Unfolding the Transition Relation) [[M]]_k
ispath :: Array Int s -> Model s -> Int -> Bool
ispath s m k = _I m (s ! 0) &&
               and [_T m (s ! i) (s ! (i+1)) | i <- [0..(k-1)]]

-- Definition 11 (Translation of an LTL Formula without a Loop) [[f]]^i_k
noloop :: Array Int s -> Int -> Int -> LTL s -> Bool
noloop s i k ltl =
  case ltl of
    P p      ->  p (s ! i)
    f :/\ g  ->  noloop s i k f && noloop s i k g
    f :\/ g  ->  noloop s i k f || noloop s i k g
    G f      ->  False
    F f      ->  or [noloop s j k f | j <- [i..k]]
    X f      ->  if i < k then noloop s (i+1) k f else False
    f `U` g  ->
       let p j = noloop s j k g && and [noloop s n k f | n <- [i..(j-1)]]
       in or [p j | j <- [i..k]]
    f `R` g ->
       let p j = noloop s j k f && and [noloop s n k g | n <- [i..j]]
       in or [p j | j <- [i..k]]

-- Definition 12 (Successor in a Loop)
succ :: Int -> Int -> Int -> Int
succ k l i = if i < k then i + 1 else l

-- Definition 13 (Translation of an LTL Formula for a Loop) l[[f]]^i_k
loop :: Array Int s -> Int -> Int -> Int -> LTL s -> Bool
loop s l i k ltl = 
  case ltl of
    P p      ->  p (s ! i)
    f :/\ g  ->  loop s l i k f && loop s l i k g
    f :\/ g  ->  loop s l i k f || loop s l i k g
    G f      ->  and [loop s l j k f | j <- [min i l .. k]]
    F f      ->  or [loop s l j k f | j <- [min i l .. k]]
    X f      ->  loop s l (succ k l i) k f
    f `U` g  ->  
      let pa j = loop s l j k g &&
                 and [loop s l n k f | n <- [i..(j-1)]]
          a = or [pa j | j <- [i..k]]
    
          pb j = loop s l j k g && 
                 and [loop s l n k f | n <- [i..k]] &&
                 and [loop s l n k f | n <- [l..(j-1)]]
          b = or [pb j | j <- [l..(i-1)]]
      in a || b
    f `R` g  -> 
      let x = and [loop s l j k g | j <- [min i l .. k]]

          pa j = loop s l j k f &&
                 and [loop s l n k g | n <- [i..j]]
          a = or [pa j | j <- [i..k]]
    
          pb j = loop s l j k f && 
                 and [loop s l n k g | n <- [i..k]] &&
                 and [loop s l n k g | n <- [l..j]]
          b = or [pb j | j <- [l..(i-1)]]
      in x || a || b

-- Definition 14 (Loop Condition) L_k
isloop :: Array Int s -> Model s -> Int -> Bool
isloop s m k = or [_T m (s ! k) (s ! l) | l <- [0..k]]
   
-- Definition 15 (General Translation) [[M,f]]_k
trans :: Array Int s -> Model s -> LTL s -> Int -> Bool
trans s m f k =
  let isp = ispath s m k
      nl = not (isloop s m k) && noloop s 0 k f
      lp = or [_T m (s ! k) (s ! l) && loop s l 0 k f | l <- [0..k]]
  in isp && (nl || lp)

-- Check whether the formula is existentially valid in the given model with
-- the given bound.
--
-- Returns
--   Nothing  - if the formula is valid
--   Just xs  - if the formula is not valid, where xs are a counterexample.
check :: (Free s) => Model s -> LTL s -> Int -> Symbolic [s]
check m f k = do
    xs <- sequence (replicate (k+1) free)
    let s = listArray (0,k) xs
    assert (trans s m f k)
    return (elems s)
    
