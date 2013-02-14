
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Smten.Semantics (
    Symbolic(), Used(), SMT(), Free(..),
    free_BoundedEnum, free_Bool, free_Integer, free_Bit,
    assert, used, use, query_Used,
    runSMT, query, nest,
 ) where

import Debug.Trace

import Data.Bits
import Data.Dynamic
import Data.Functor
import Data.Maybe
import Data.Unique

import qualified Data.Map as Map

import Smten.Bit

-- Model for the semantics of the Smten SMT API in Haskell.
-- Uses a list monad internally rather than SMT.
--
-- Differences between this and Smten in practice:
--  * this implementation should be much slower
--  * this implementation may not terminate where the SMT version may
--  * this implementation gives only one of the possible valid answers. An SMT
--  version may give another.

type Context = Unique
type Contexts = [Unique]

type DMap = Map.Map Integer Dynamic

data SE = SE {
    se_ctx :: Contexts,
    se_map :: DMap
} deriving (Show)

-- 'Symbolic a' represents a set (here list) of possible values of type 'a' in
-- an environment of use contexts
data Symbolic a = Symbolic {
    runSymbolic :: SE -> [a]
}

free_BoundedEnum :: (Bounded a, Enum a) => Symbolic a
free_BoundedEnum = Symbolic $ const [minBound .. maxBound]

free_Bool :: Symbolic Bool
free_Bool = free_BoundedEnum

free_Integer :: Symbolic Integer
free_Integer =
 -- We interleave the positive and negative integers so the integers with
 -- smallest magnitudes are at the front of the list.
 let interleave :: [a] -> [a] -> [a]
     interleave x [] = x
     interleave [] y = y
     interleave (x:xs) (y:ys) = x:y:interleave xs ys
 in Symbolic (const $ interleave [0..] (map negate [1..]))

-- Create a free Bit of given width.
free_Bit :: Integer -> Symbolic Bit
free_Bit w = Symbolic (const [bv_make w v | v <- [0 .. (shiftL 1 (fromInteger w)) - 1]])

class Free a where
    free :: Symbolic a

instance Free Bool where
    free = free_Bool

instance Free Integer where
    free = free_Integer

instance Functor Symbolic where
    fmap f x = do
         v <- x
         return (f v)

instance Monad Symbolic where
    return x = Symbolic $ const [x]
    (>>=) x f = Symbolic $ \e ->
        concat [runSymbolic (f v) e | v <- runSymbolic x e]

assert :: Bool -> Symbolic ()
assert True = Symbolic $ const [()]
assert False = Symbolic $ const []

data Used a = Used Context Integer
    deriving (Show)

used :: (Typeable a) => Used a -> Symbolic a
used (Used ctx r) =  Symbolic $ \e ->
    if ctx `elem` (se_ctx e)
        then [dmaplookup r (se_map e)]
        else error "used: invalid context"

dmaplookup :: (Typeable a) => Integer -> Map.Map Integer Dynamic -> a
dmaplookup k m =
  case (Map.lookup k m) of
      Nothing -> error $ "dmaplookup not found: " ++ show (k, m)
      Just d -> case (fromDynamic d) of
                    Nothing -> error $ "dmaplookup bad type: " ++ show (k, m)
                    Just v -> v

data SS = SS {
    ss_ctx :: Contexts,
    ss_maps :: [DMap],
    ss_nref :: Integer
} deriving (Show)

instance Show Unique where
    show = const "?"
    

data SMT a = SMT {
    runSMT_ :: SS -> IO (a, SS)
}

instance Monad SMT where
    return x = SMT $ \s -> return (x, s)
    (>>=) x f = SMT $ \s -> do
        (v, s') <- runSMT_ x s
        runSMT_ (f v) s'

nest :: SMT a -> SMT a
nest x = SMT $ \s -> do
    ctx <- newUnique
    (v, _) <- runSMT_ x (s { ss_ctx = ctx : ss_ctx s })
    return (v, s)

use :: (Typeable a) => Symbolic a -> SMT (Used a)
use x = SMT $ \s -> do
    let nref = ss_nref s
        ms' = do
           m <- ss_maps s
           v <- runSymbolic x (SE (ss_ctx s) m)
           return (Map.insert nref (toDyn v) m)
        ref = Used (head $ ss_ctx s) nref
        s' = s { ss_nref = nref+1, ss_maps = ms' }
    return (ref, s')

query_Used :: (Typeable a) => Used a -> SMT (Maybe a) 
query_Used (Used ctx r) = SMT $ \s ->
    let vs = [dmaplookup r m | m <- ss_maps s]
        result = case vs of
                    x:_ -> Just x
                    [] -> Nothing
    in return (result, s)

runSMT :: SMT a -> IO a
runSMT x = fst <$> runSMT_ x (SS [] [Map.empty] 0)

query :: (Typeable a) => Symbolic a -> SMT (Maybe a)
query x = nest $ use x >>= query_Used

