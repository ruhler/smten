
module Smten.SMT.Assert (Smten.SMT.Assert.assert) where

import Control.Monad.Reader
import System.Mem.StableName

import qualified Data.HashTable.IO as H

import Smten.Bit
import qualified Smten.Runtime.SmtenHS as S
import Smten.Runtime.SmtenHS (Cases(..))
import Smten.SMT.Solver.Static as ST
import Smten.SMT.FreeID

type BoolCache exp = H.BasicHashTable (StableName S.Bool) exp
type IntegerCache exp = H.BasicHashTable (StableName S.Integer) exp
type BitCache exp = H.BasicHashTable (StableName S.Bit) exp

data AR ctx exp = AR {
  ar_ctx :: ctx,
  ar_bools :: BoolCache exp,
  ar_integers :: IntegerCache exp,
  ar_bits :: BitCache exp
}

type AM ctx exp = ReaderT (AR ctx exp) IO

assert :: (Solver ctx exp) => ctx -> S.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    bc <- H.new
    ic <- H.new
    btc <- H.new
    e <- runReaderT (def_bool ctx p) (AR ctx bc ic btc)
    ST.assert ctx e

use_bool :: (Solver ctx exp) => S.Bool -> AM ctx exp exp
use_bool b = do
    nm <- liftIO $ makeStableName $! b
    bc <- asks ar_bools
    found <- liftIO $ H.lookup bc nm
    case found of
        Just v -> return v
        Nothing -> do
            ctx <- asks ar_ctx
            v <- def_bool ctx b
            liftIO $ H.insert bc nm v
            return v

use_int :: (Solver ctx exp) => S.Integer -> AM ctx exp exp
use_int i = do
    nm <- liftIO $ makeStableName $! i
    ic <- asks ar_integers
    found <- liftIO $ H.lookup ic nm
    case found of
        Just v -> return v
        Nothing -> do
            ctx <- asks ar_ctx
            v <- def_int ctx i
            liftIO $ H.insert ic nm v
            return v

use_bit :: (Solver ctx exp) => S.Bit -> AM ctx exp exp
use_bit i = do
    nm <- liftIO $ makeStableName $! i
    ic <- asks ar_bits
    found <- liftIO $ H.lookup ic nm
    case found of
        Just v -> return v
        Nothing -> do
            ctx <- asks ar_ctx
            v <- def_bit ctx i
            liftIO $ H.insert ic nm v
            return v

def_bool :: (Solver ctx exp) => ctx -> S.Bool -> AM ctx exp exp
def_bool ctx S.True = liftIO $ bool ctx True
def_bool ctx S.False = liftIO $ bool ctx False
def_bool ctx (S.Bool_Var id) = liftIO $ var ctx (freenm id)
def_bool ctx (S.Bool_EqInteger a b) = int_binary (eq_integer ctx) a b
def_bool ctx (S.Bool_LeqInteger a b) = int_binary (leq_integer ctx) a b
def_bool ctx (S.Bool_EqBit a b) = bit_binary (eq_bit ctx) a b
def_bool ctx (S.Bool_LeqBit a b) = bit_binary (leq_bit ctx) a b
def_bool ctx (S.Bool_Ite p a b) = do
    p' <- use_bool p
    a' <- use_bool a
    b' <- use_bool b
    liftIO $ ite_bool ctx p' a' b'
def_bool ctx (S.Bool_Prim _ c) = decases_bool ctx c

decases_bool :: (Solver ctx exp) => ctx -> Cases S.Bool -> AM ctx exp exp
decases_bool ctx (Concrete c) = use_bool c
decases_bool ctx (Switch p a b) = do
    p' <- use_bool p
    a' <- decases_bool ctx a
    b' <- decases_bool ctx b
    liftIO $ ite_bool ctx p' a' b'

def_int :: (Solver ctx exp) => ctx -> S.Integer -> AM ctx exp exp
def_int ctx (S.Integer i) = liftIO $ integer ctx i
def_int ctx (S.Integer_Add a b) = int_binary (add_integer ctx) a b
def_int ctx (S.Integer_Sub a b) = int_binary (sub_integer ctx) a b
def_int ctx (S.Integer_Ite p a b) = do
    p' <- use_bool p
    a' <- use_int a
    b' <- use_int b
    liftIO $ ite_integer ctx p' a' b'
def_int ctx (S.Integer_Var id) = liftIO $ var ctx (freenm id)
def_int ctx (S.Integer_Prim _ c) = decases_int ctx c

decases_int :: (Solver ctx exp) => ctx -> Cases S.Integer -> AM ctx exp exp
decases_int ctx (Concrete c) = use_int c
decases_int ctx (Switch p a b) = do
    p' <- use_bool p
    a' <- decases_int ctx a
    b' <- decases_int ctx b
    liftIO $ ite_integer ctx p' a' b'

int_binary :: (Solver ctx exp) => (exp -> exp -> IO exp) -> S.Integer -> S.Integer -> AM ctx exp exp
int_binary f a b = do
    a' <- use_int a
    b' <- use_int b
    liftIO $ f a' b'

def_bit :: (Solver ctx exp) => ctx -> S.Bit -> AM ctx exp exp
def_bit ctx (S.Bit x) = liftIO $ bit ctx (bv_width x) (bv_value x)
def_bit ctx (S.Bit_Add a b) = bit_binary (add_bit ctx) a b
def_bit ctx (S.Bit_Sub a b) = bit_binary (sub_bit ctx) a b
def_bit ctx (S.Bit_Mul a b) = bit_binary (mul_bit ctx) a b
def_bit ctx (S.Bit_Or a b) = bit_binary (or_bit ctx) a b
def_bit ctx (S.Bit_Ite p a b) = do
    p' <- use_bool p
    a' <- use_bit a
    b' <- use_bit b
    liftIO $ ite_bit ctx p' a' b'
def_bit ctx (S.Bit_Var id) = liftIO $ var ctx (freenm id)
def_bit ctx (S.Bit_Prim _ c) = decases_bit ctx c

decases_bit :: (Solver ctx exp) => ctx -> Cases S.Bit -> AM ctx exp exp
decases_bit ctx (Concrete c) = use_bit c
decases_bit ctx (Switch p a b) = do
    p' <- use_bool p
    a' <- decases_bit ctx a
    b' <- decases_bit ctx b
    liftIO $ ite_bit ctx p' a' b'

bit_binary :: (Solver ctx exp) => (exp -> exp -> IO exp) -> S.Bit -> S.Bit -> AM ctx exp exp
bit_binary f a b = do
    a' <- use_bit a
    b' <- use_bit b
    liftIO $ f a' b'

