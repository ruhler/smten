
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Assert (Smten.Runtime.Assert.assert) where

import Control.Monad.Reader
import System.Mem.StableName

import qualified Data.HashTable.IO as H

import Smten.Runtime.AssertCache as AC
import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.SolverAST as ST

type Cache a exp = H.BasicHashTable (StableName a) exp
type Vars = H.BasicHashTable FreeID Type

data AR ctx exp = AR {
  ar_ctx :: ctx,
  ar_cachekey :: AC.AssertCacheKey,
  ar_intcache :: Cache IntegerFF exp,
  ar_bitcache :: Cache BitFF exp,

  -- Track the user-visible SMT variables used in the assertion.
  ar_vars :: Vars
}

type AM ctx exp = ReaderT (AR ctx exp) IO

class Supported a where
    define :: (SolverAST ctx exp) => ctx -> a -> AM ctx exp exp

    -- Define the value if it hasn't yet been, but look up in the
    -- cache to see if it's already been defined first.
    use :: (SolverAST ctx exp) => a -> AM ctx exp exp

assert :: (SolverAST ctx exp) => ctx -> BoolFF -> IO [(FreeID, Type)]
assert ctx p = {-# SCC "Assert" #-} do
    key <- AC.newKey
    bitc <- H.new
    intc <- H.new
    vars <- H.new
    e <- runReaderT (define ctx p) (AR ctx key bitc intc vars)
    ST.assert ctx e
    H.toList vars

use_xx :: (SolverAST ctx exp, Supported a) => Cache a exp -> a -> AM ctx exp exp
use_xx c x = do
    nm <- liftIO $ makeStableName $! x
    found <- liftIO $ H.lookup c nm
    case found of
        Just v -> {-# SCC "CacheHit" #-} return v
        Nothing -> {-# SCC "CacheMiss" #-} do
            ctx <- asks ar_ctx
            v <- define ctx x
            liftIO $ H.insert c nm v
            return v

unary :: (SolverAST ctx exp, Supported a) => (exp -> IO exp) -> a -> AM ctx exp exp
unary f a = do
    a' <- use a
    liftIO $ f a'

binary :: (SolverAST ctx exp, Supported a) => (exp -> exp -> IO exp) -> a -> a -> AM ctx exp exp
binary f a b = do
    a' <- use a
    b' <- use b
    liftIO $ f a' b'

trinary :: (SolverAST ctx exp, Supported a) => (exp -> exp -> exp -> IO exp) -> a -> a -> a -> AM ctx exp exp
trinary f a b c = do
    a' <- use a
    b' <- use b
    c' <- use c
    liftIO $ f a' b' c'

instance Supported BoolFF where
    define ctx x = use x

    use x = do
      key <- asks ar_cachekey
      ctx <- asks ar_ctx
      case x of
        TrueFF -> liftIO $ bool ctx True
        FalseFF -> liftIO $ bool ctx False
        IteFF p a b c -> AC.cached c key (trinary (ite_bool ctx) p a b)
        AndFF a b c -> AC.cached c key (binary (and_bool ctx) a b)
        OrFF a b c -> AC.cached c key (binary (or_bool ctx) a b)
        NotFF a c -> AC.cached c key (unary (not_bool ctx) a)
        VarFF id -> uservar ctx id BoolT
        Eq_IntegerFF a b c -> AC.cached c key (binary (eq_integer ctx) a b)
        Leq_IntegerFF a b c -> AC.cached c key (binary (leq_integer ctx) a b)
        Eq_BitFF a b c -> AC.cached c key (binary (eq_bit ctx) a b)
        Leq_BitFF a b c -> AC.cached c key (binary (leq_bit ctx) a b)

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
       
instance Supported IntegerFF where
    define ctx (IntegerFF i) = liftIO $ integer ctx i
    define ctx (Add_IntegerFF a b) = binary (add_integer ctx) a b
    define ctx (Sub_IntegerFF a b) = binary (sub_integer ctx) a b
    define ctx (Ite_IntegerFF p a b) = do
        p' <- use p
        a' <- use a
        b' <- use b
        liftIO $ ite_integer ctx p' a' b'
    define ctx (Var_IntegerFF id) = uservar ctx id IntegerT

    use x = {-# SCC "UseInt" #-} do
      c <- asks ar_intcache
      use_xx c x

instance Supported BitFF where
    define ctx (BitFF x) = liftIO $ bit ctx (bv_width x) (bv_value x)
    define ctx (Add_BitFF a b) = binary (add_bit ctx) a b
    define ctx (Sub_BitFF a b) = binary (sub_bit ctx) a b
    define ctx (Mul_BitFF a b) = binary (mul_bit ctx) a b
    define ctx (Or_BitFF a b) = binary (or_bit ctx) a b
    define ctx (And_BitFF a b) = binary (and_bit ctx) a b
    define ctx (Shl_BitFF w a b) = binary (shl_bit ctx w) a b
    define ctx (Lshr_BitFF w a b) = binary (lshr_bit ctx w) a b
    define ctx (Concat_BitFF a b) = binary (concat_bit ctx) a b
    define ctx (Not_BitFF a) = unary (not_bit ctx) a
    define ctx (SignExtend_BitFF fr to a) = unary (sign_extend_bit ctx fr to) a
    define ctx (Extract_BitFF hi lo a) = unary (extract_bit ctx hi lo) a
    define ctx (Ite_BitFF p a b) = do
        p' <- use p
        a' <- use a
        b' <- use b
        liftIO $ ite_bit ctx p' a' b'
    define ctx (Var_BitFF w id) = uservar ctx id (BitT w)

    use x = {-# SCC "UseBit" #-} do
      c <- asks ar_bitcache
      use_xx c x
