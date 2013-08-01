
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
  ar_cache :: Cache exp,

  -- If we are currently defining the expression for a bit-vector object, this
  -- holds the width of that object.
  ar_bitwidth :: Integer
}

type AM ctx exp = ReaderT (AR ctx exp) IO

withbitwidth :: Integer -> AM ctx exp a -> AM ctx exp a
withbitwidth i = local (\r -> r { ar_bitwidth = i })

class Supported a where
    define :: (SolverAST ctx exp) => ctx -> a -> AM ctx exp exp

assert :: (SolverAST ctx exp) => ctx -> S.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    c <- H.new
    e <- runReaderT (define ctx p) (AR ctx c (error "ar_bitwidth not set!"))
    ST.assert ctx e

use :: (SolverAST ctx exp, Supported a) => a -> AM ctx exp exp
use x = do
    nm <- liftIO $ makeStableName $! (unsafeCoerce x)
    c <- asks ar_cache
    found <- liftIO $ H.lookup c nm
    case found of
        Just v -> return v
        Nothing -> do
            ctx <- asks ar_ctx
            v <- define ctx x
            liftIO $ H.insert c nm v
            return v

unary :: (SolverAST ctx exp, Supported a) => (exp -> IO exp) -> a -> AM ctx exp exp
unary f a = do
    a' <- use a
    liftIO $ f a'

binary :: (SolverAST ctx exp, Supported a, Supported b) => (exp -> exp -> IO exp) -> a -> b -> AM ctx exp exp
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
    define ctx (S.Bool_Prim _ c) = define ctx c
    define ctx (S.Bool_Err msg) = liftIO $ do
       id <- fresh
       declare ctx S.BoolT (freenm id)
       var ctx (freenm id)
       
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
    define ctx (S.Integer_Prim _ c) = define ctx c
    define ctx (S.Integer_Err msg) = liftIO $ do
        id <- fresh
        declare ctx S.IntegerT (freenm id)
        var ctx (freenm id)

instance Supported (S.Bit n) where
    define ctx (S.Bit x) = liftIO $ bit ctx (bv_width x) (bv_value x)
    define ctx (S.Bit_Add a b) = binary (add_bit ctx) a b
    define ctx (S.Bit_Sub a b) = binary (sub_bit ctx) a b
    define ctx (S.Bit_Mul a b) = binary (mul_bit ctx) a b
    define ctx (S.Bit_Or a b) = binary (or_bit ctx) a b
    define ctx (S.Bit_And a b) = binary (and_bit ctx) a b
    define ctx (S.Bit_Shl a b) = binary (shl_bit ctx) a b
    define ctx (S.Bit_Lshr a b) = binary (lshr_bit ctx) a b
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
    define ctx (S.Bit_Var id) = liftIO $ var ctx (freenm id)
    define ctx (S.Bit_Prim _ c) = define ctx c
    define ctx (S.Bit_Err msg) = do
      w <- asks ar_bitwidth
      liftIO $ do
        id <- fresh
        declare ctx (S.BitT w) (freenm id)
        var ctx (freenm id)

