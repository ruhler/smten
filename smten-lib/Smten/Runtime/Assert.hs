
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Assert (Smten.Runtime.Assert.assert) where

import GHC.Base
import Unsafe.Coerce

import Control.Monad.Reader
import System.Mem.StableName

import qualified Data.HashTable.IO as H

import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import qualified Smten.Runtime.Types as S
import Smten.Runtime.SolverAST as ST

type Cache exp = H.BasicHashTable (StableName Any) exp

data AR ctx exp = AR {
  ar_ctx :: ctx,
  ar_boolcache :: Cache exp,
  ar_intcache :: Cache exp,
  ar_bitcache :: Cache exp,

  -- If we are currently defining the expression for a bit-vector object, this
  -- holds the width of that object.
  ar_bitwidth :: Integer
}

type AM ctx exp = ReaderT (AR ctx exp) IO

withbitwidth :: Integer -> AM ctx exp a -> AM ctx exp a
withbitwidth i = local (\r -> r { ar_bitwidth = i })

class Supported a where
    define :: (SolverAST ctx exp) => ctx -> a -> AM ctx exp exp

    -- Retrieve the cache associated with objects of this data type.
    -- The value of first argument is ignored.
    cache :: a -> AM ctx exp (Cache exp)

assert :: (SolverAST ctx exp) => ctx -> S.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    boolc <- H.new
    bitc <- H.new
    intc <- H.new
    e <- runReaderT (define ctx p) (AR ctx boolc bitc intc (error "ar_bitwidth not set!"))
    ST.assert ctx e

{-# SPECIALIZE use :: (SolverAST ctx exp) => S.Bool -> AM ctx exp exp #-}
{-# SPECIALIZE use :: (SolverAST ctx exp) => S.Integer -> AM ctx exp exp #-}
{-# SPECIALIZE use :: (SolverAST ctx exp) => S.Bit n -> AM ctx exp exp #-}
use :: (SolverAST ctx exp, Supported a) => a -> AM ctx exp exp
use x = do
    nm <- liftIO $ makeStableName $! (unsafeCoerce x)
    c <- cache x
    found <- liftIO $ H.lookup c nm
    case found of
        Just v -> return v
        Nothing -> do
            ctx <- asks ar_ctx
            v <- define ctx x
            liftIO $ H.insert c nm v
            return v

{-# SPECIALIZE unary :: (SolverAST ctx exp) => (exp -> IO exp) -> S.Bool -> AM ctx exp exp #-}
{-# SPECIALIZE unary :: (SolverAST ctx exp) => (exp -> IO exp) -> S.Integer -> AM ctx exp exp #-}
{-# SPECIALIZE unary :: (SolverAST ctx exp) => (exp -> IO exp) -> S.Bit n -> AM ctx exp exp #-}
unary :: (SolverAST ctx exp, Supported a) => (exp -> IO exp) -> a -> AM ctx exp exp
unary f a = do
    a' <- use a
    liftIO $ f a'

{-# SPECIALIZE binary :: (SolverAST ctx exp) => (exp -> exp -> IO exp) -> S.Bool -> S.Bool -> AM ctx exp exp #-}
{-# SPECIALIZE binary :: (SolverAST ctx exp) => (exp -> exp -> IO exp) -> S.Integer -> S.Integer -> AM ctx exp exp #-}
{-# SPECIALIZE binary :: (SolverAST ctx exp) => (exp -> exp -> IO exp) -> S.Bit n -> S.Bit n -> AM ctx exp exp #-}
binary :: (SolverAST ctx exp, Supported a) => (exp -> exp -> IO exp) -> a -> a -> AM ctx exp exp
binary f a b = do
    a' <- use a
    b' <- use b
    liftIO $ f a' b'

instance Supported S.Bool where
    define ctx S.True = liftIO $ bool ctx True
    define ctx S.False = liftIO $ bool ctx False
    define ctx (S.Bool_Var id) = liftIO $ var ctx (freenm id)
    define ctx (S.Bool_EqInteger a b) = binary (eq_integer ctx) a b
    define ctx (S.Bool_LeqInteger a b) = binary (leq_integer ctx) a b
    define ctx (S.Bool_EqBit w a b) = withbitwidth w $ binary (eq_bit ctx) a b
    define ctx (S.Bool_LeqBit w a b) = withbitwidth w $ binary (leq_bit ctx) a b
    define ctx (S.Bool_Ite p a b) = do
        p' <- use p
        a' <- use a
        b' <- use b
        liftIO $ ite_bool ctx p' a' b'

    define ctx (S.Bool_And a b) = binary (and_bool ctx) a b
    define ctx (S.Bool_Not a) = unary (not_bool ctx) a
    define ctx (S.Bool_Err msg) = liftIO $ do
       id <- fresh
       declare ctx S.BoolT (freenm id)
       var ctx (freenm id)

    cache _ = asks ar_boolcache
       
instance Supported S.Integer where
    define ctx (S.Integer i) = liftIO $ integer ctx i
    define ctx (S.Integer_Add a b) = binary (add_integer ctx) a b
    define ctx (S.Integer_Sub a b) = binary (sub_integer ctx) a b
    define ctx (S.Integer_Ite p a b) = do
        p' <- use p
        a' <- use a
        b' <- use b
        liftIO $ ite_integer ctx p' a' b'
    define ctx (S.Integer_Var id) = liftIO $ var ctx (freenm id)
    define ctx (S.Integer_Err msg) = liftIO $ do
        id <- fresh
        declare ctx S.IntegerT (freenm id)
        var ctx (freenm id)

    cache _ = asks ar_intcache

instance Supported (S.Bit n) where
    define ctx (S.Bit x) = liftIO $ bit ctx (bv_width x) (bv_value x)
    define ctx (S.Bit_Add a b) = binary (add_bit ctx) a b
    define ctx (S.Bit_Sub a b) = binary (sub_bit ctx) a b
    define ctx (S.Bit_Mul a b) = binary (mul_bit ctx) a b
    define ctx (S.Bit_Or a b) = binary (or_bit ctx) a b
    define ctx (S.Bit_And a b) = binary (and_bit ctx) a b
    define ctx (S.Bit_Shl a b) = do
        w <- asks ar_bitwidth
        binary (shl_bit ctx w) a b
    define ctx (S.Bit_Lshr a b) = do
        w <- asks ar_bitwidth
        binary (lshr_bit ctx w) a b
    define ctx (S.Bit_Concat wa a b) = do
        w <- asks ar_bitwidth
        a' <- withbitwidth wa $ use a
        b' <- withbitwidth (w - wa) $ use b
        liftIO $ concat_bit ctx a' b'
    define ctx (S.Bit_Not a) = unary (not_bit ctx) a
    define ctx (S.Bit_Extract wa hi lo a) = withbitwidth wa $ unary (extract_bit ctx hi lo) a
    define ctx (S.Bit_SignExtend by a) = do
        to <- asks ar_bitwidth
        let fr = to - by
        withbitwidth fr $ unary (sign_extend_bit ctx fr to) a
    define ctx (S.Bit_Ite p a b) = do
        p' <- use p
        a' <- use a
        b' <- use b
        liftIO $ ite_bit ctx p' a' b'
    define ctx (S.Bit_Var _ id) = liftIO $ var ctx (freenm id)
    define ctx (S.Bit_Err msg) = do
      w <- asks ar_bitwidth
      liftIO $ do
        id <- fresh
        declare ctx (S.BitT w) (freenm id)
        var ctx (freenm id)

    cache _ = asks ar_bitcache
