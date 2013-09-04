
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
import Smten.Data.Functor((<$>))
import Smten.Symbolic

data Model s = Model {
    initial :: s -> Bool,
    transition :: s -> s -> Bool
}

data LTL s =
    Prop (s -> Bool)            -- p
  | Both (LTL s) (LTL s)        -- f and g
  | Either (LTL s) (LTL s)      -- f or g
  | Globally (LTL s)            -- G f
  | Eventually (LTL s)          -- F f
  | Next (LTL s)                -- X f
  | Until (LTL s) (LTL s)       -- f U g
  | Release (LTL s) (LTL s)     -- f R g
    

-- Create a free sequence of (k+1) states for bound k.
freeseq :: (Free s) => Int -> Symbolic (Array Int s)
freeseq k = listArray (0,k) <$> sequence (replicate (k+1) free)

-- Definition 10 (Unfolding the Transition Relation)
--    [[M]]_k
ispath :: Array Int s -> Model s -> Int -> Bool
ispath s m k = initial m (s ! 0) &&
               and [transition m (s ! i) (s ! (i+1)) | i <- [0..(k-1)]]

-- Definition 11 (Translation of an LTL Formula without a Loop)
--     [[f]]^i_k
noloop :: Array Int s -> Int -> Int -> LTL s -> Bool
noloop s i k ltl
 | k < i = error "noloop: k < i"
 | Prop p <- ltl = p (s ! i)
 | Both f g <- ltl = noloop s i k f && noloop s i k g
 | Either f g <- ltl = noloop s i k f || noloop s i k g
 | Globally f <- ltl = False
 | Eventually f <- ltl = or [noloop s j k f | j <- [i..k]]
 | Next f <- ltl = if i < k then noloop s (i+1) k f else False
 | Until f g <- ltl =
     let p j = noloop s j k g && and [noloop s n k f | n <- [i..(j-1)]]
     in or [p j | j <- [i..k]]
 | Release f g <- ltl =
     let p j = noloop s j k f && and [noloop s n k g | n <- [i..j]]
     in or [p j | j <- [i..k]]

-- Definition 12 (Successor in a Loop)
succ :: Int -> Int -> Int -> Int
succ k l i
  | i < k = i+1
  | otherwise = l

-- Definition 13 (Translation of an LTL Formula for a Loop)
--     l[[f]]^i_k
loop :: Array Int s -> Int -> Int -> Int -> LTL s -> Bool
loop s l i k ltl
 | k < i = error "loop: k < i"
 | Prop p <- ltl = p (s ! i)
 | Both f g <- ltl = loop s l i k f && loop s l i k g
 | Either f g <- ltl = loop s l i k f || loop s l i k g
 | Globally f <- ltl = and [loop s l j k f | j <- [min i l .. k]]
 | Eventually f <- ltl = or [loop s l j k f | j <- [min i l .. k]]
 | Next f <- ltl = loop s l (succ k l i) k f
 | Until f g <- ltl = 
     let pa j = loop s l j k g &&
                and [loop s l n k f | n <- [i..(j-1)]]
         a = or [pa j | j <- [i..k]]
   
         pb j = loop s l j k g && 
                and [loop s l n k f | n <- [i..k]] &&
                and [loop s l n k f | n <- [l..(j-1)]]
         b = or [pb j | j <- [l..(i-1)]]
     in a || b
 | Release f g <- ltl =
     let x = and [loop s l j k g | j <- [min i l .. k]]

         pa j = loop s l j k f &&
                and [loop s l n k g | n <- [i..j]]
         a = or [pa j | j <- [i..k]]
   
         pb j = loop s l j k f && 
                and [loop s l n k g | n <- [i..k]] &&
                and [loop s l n k g | n <- [l..j]]
         b = or [pb j | j <- [l..(i-1)]]
     in x || a || b

-- Definition 14 (Loop Condition)
--    L_k
isloop :: Array Int s -> Model s -> Int -> Bool
isloop s m k = or [transition m (s ! k) (s ! l) | l <- [0..k]]
   
-- Definition 15 (General Translation)
--  [[M,f]]_k
trans :: Array Int s -> Model s -> LTL s -> Int -> Bool
trans s m f k =
  let isp = ispath s m k
      nl = not (isloop s m k) && noloop s 0 k f
      lp = or [transition m (s ! k) (s ! l) && loop s l 0 k f | l <- [0..k]]
  in isp && (nl || lp)

-- Check whether the formula is existentially valid in the given model with
-- the given bound.
--
-- Returns
--   Nothing  - if the formula is valid
--   Just xs  - if the formula is not valid, where xs are a counterexample.
check :: (Free s) => Model s -> LTL s -> Int -> Symbolic [s]
check m f k = do
    s <- freeseq k
    assert (trans s m f k)
    return (elems s)
    
