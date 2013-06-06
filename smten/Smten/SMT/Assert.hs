
module Smten.SMT.Assert (Smten.SMT.Assert.assert) where

import Control.Monad.Reader
import System.Mem.StableName

import qualified Data.HashTable.IO as H

import qualified Smten.Runtime.SmtenHS as S
import Smten.SMT.AST as AST
import Smten.SMT.FreeID

type BoolCache exp = H.BasicHashTable (StableName S.Bool) exp
type IntegerCache exp = H.BasicHashTable (StableName S.Integer) exp

data AR ctx exp = AR {
  ar_ctx :: ctx,
  ar_bools :: BoolCache exp,
  ar_integers :: IntegerCache exp
}

type AM ctx exp = ReaderT (AR ctx exp) IO

assert :: (AST ctx exp) => ctx -> S.Bool -> IO ()
assert ctx p = {-# SCC "Assert" #-} do
    bc <- H.new
    ic <- H.new
    e <- runReaderT (def_bool ctx p) (AR ctx bc ic)
    AST.assert ctx e

use_bool :: (AST ctx exp) => S.Bool -> AM ctx exp exp
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

use_int :: (AST ctx exp) => S.Integer -> AM ctx exp exp
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

def_bool :: (AST ctx exp) => ctx -> S.Bool -> AM ctx exp exp
def_bool ctx S.True = liftIO $ bool ctx True
def_bool ctx S.False = liftIO $ bool ctx False
def_bool ctx (S.BoolVar id) = liftIO $ var ctx (freenm id)
def_bool ctx (S.BoolMux p a b) = do
    p' <- use_bool p
    a' <- use_bool a
    b' <- use_bool b
    liftIO $ ite ctx p' a' b'
def_bool ctx (S.Bool__EqInteger a b) = int_binary (eq_integer ctx) a b
def_bool ctx (S.Bool__LeqInteger a b) = int_binary (leq_integer ctx) a b

def_int :: (AST ctx exp) => ctx -> S.Integer -> AM ctx exp exp
def_int ctx (S.Integer i) = liftIO $ integer ctx i
def_int ctx (S.Integer_Add a b) = int_binary (add_integer ctx) a b
def_int ctx (S.Integer_Sub a b) = int_binary (sub_integer ctx) a b
def_int ctx (S.IntegerMux p a b) = do
    p' <- use_bool p
    a' <- use_int a
    b' <- use_int b
    liftIO $ ite ctx p' a' b'
def_int ctx (S.IntegerVar id) = liftIO $ var ctx (freenm id)

int_binary :: (AST ctx exp) => (exp -> exp -> IO exp) -> S.Integer -> S.Integer -> AM ctx exp exp
int_binary f a b = do
    a' <- use_int a
    b' <- use_int b
    liftIO $ f a' b'

