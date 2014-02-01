
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Assert (Smten.Runtime.Assert.assert) where

import Control.Monad.Reader

import qualified Data.HashTable.IO as H

import Smten.Runtime.AssertCache as AC
import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.SolverAST as ST

type Vars = H.BasicHashTable FreeID Type

data AR ctx exp = AR {
  ar_ctx :: ctx,
  ar_cachekey :: AC.AssertCacheKey,

  -- Track the user-visible SMT variables used in the assertion.
  ar_vars :: Vars
}

type AM ctx exp = ReaderT (AR ctx exp) IO

class Supported a where
    build :: (SolverAST ctx exp) => a -> AM ctx exp exp

assert :: (SolverAST ctx exp) => ctx -> BoolFF -> IO [(FreeID, Type)]
assert ctx p = {-# SCC "Assert" #-} do
    key <- AC.newKey
    vars <- H.new
    e <- runReaderT (build p) (AR ctx key vars)
    ST.assert ctx e
    H.toList vars

unary :: (SolverAST ctx exp, Supported a) => (exp -> IO exp) -> a -> AM ctx exp exp
unary f a = do
    a' <- build a
    liftIO $ f a'

binary :: (SolverAST ctx exp, Supported a) => (exp -> exp -> IO exp) -> a -> a -> AM ctx exp exp
binary f a b = do
    a' <- build a
    b' <- build b
    liftIO $ f a' b'

trinary :: (SolverAST ctx exp, Supported a, Supported b) => (exp -> exp -> exp -> IO exp) -> a -> b -> b -> AM ctx exp exp
trinary f a b c = do
    a' <- build a
    b' <- build b
    c' <- build c
    liftIO $ f a' b' c'

uservar :: (SolverAST ctx exp) => ctx -> FreeID -> Type -> AM ctx exp exp
uservar ctx id ty = do
  vars <- asks ar_vars
  liftIO $ do
    m <- H.lookup vars id
    case m of
        Nothing -> do
           declare ctx ty (freenm id)
           H.insert vars id ty
        Just _ -> return ()
    var ctx (freenm id)
       
instance Supported BoolFF where
    build x = do
      key <- asks ar_cachekey
      ctx <- asks ar_ctx
      case x of
        TrueFF -> liftIO $ bool ctx True
        FalseFF -> liftIO $ bool ctx False
        IteFF p a b c -> AC.cached c key (trinary (ite_bool ctx) p a b)
        AndFF a b c -> AC.cached c key (binary (and_bool ctx) a b)
        OrFF a b c -> AC.cached c key (binary (or_bool ctx) a b)
        NotFF a c -> AC.cached c key (unary (not_bool ctx) a)
        VarFF id c -> AC.cached c key (uservar ctx id BoolT)
        Eq_IntegerFF a b c -> AC.cached c key (binary (eq_integer ctx) a b)
        Leq_IntegerFF a b c -> AC.cached c key (binary (leq_integer ctx) a b)
        Eq_BitFF a b c -> AC.cached c key (binary (eq_bit ctx) a b)
        Leq_BitFF a b c -> AC.cached c key (binary (leq_bit ctx) a b)

instance Supported IntegerFF where
    build x = do
      key <- asks ar_cachekey
      ctx <- asks ar_ctx
      case x of
        IntegerFF i -> liftIO $ integer ctx i
        Add_IntegerFF a b c -> AC.cached c key (binary (add_integer ctx) a b)
        Sub_IntegerFF a b c -> AC.cached c key (binary (sub_integer ctx) a b)
        Ite_IntegerFF p a b c -> AC.cached c key (trinary (ite_integer ctx) p a b)
        Var_IntegerFF id c -> AC.cached c key (uservar ctx id IntegerT)

instance Supported BitFF where
    build x = do
      key <- asks ar_cachekey
      ctx <- asks ar_ctx
      case x of
        BitFF x -> liftIO $ bit ctx (bv_width x) (bv_value x)
        Add_BitFF a b c -> AC.cached c key (binary (add_bit ctx) a b)
        Sub_BitFF a b c -> AC.cached c key (binary (sub_bit ctx) a b)
        Mul_BitFF a b c -> AC.cached c key (binary (mul_bit ctx) a b)
        Or_BitFF a b c -> AC.cached c key (binary (or_bit ctx) a b)
        And_BitFF a b c -> AC.cached c key (binary (and_bit ctx) a b)
        Shl_BitFF w a b c -> AC.cached c key (binary (shl_bit ctx w) a b)
        Lshr_BitFF w a b c -> AC.cached c key (binary (lshr_bit ctx w) a b)
        Concat_BitFF a b c -> AC.cached c key (binary (concat_bit ctx) a b)
        Not_BitFF a c -> AC.cached c key (unary (not_bit ctx) a)
        SignExtend_BitFF fr to a c -> AC.cached c key (unary (sign_extend_bit ctx fr to) a)
        Extract_BitFF hi lo a c -> AC.cached c key (unary (extract_bit ctx hi lo) a)
        Ite_BitFF p a b c -> AC.cached c key (trinary (ite_bit ctx) p a b)
        Var_BitFF w id c -> AC.cached c key (uservar ctx id (BitT w))
