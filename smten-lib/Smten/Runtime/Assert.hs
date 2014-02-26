
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

assert :: (SolverAST ctx exp) => ctx -> BoolFF -> IO [(FreeID, Type)]
assert ctx p = {-# SCC "Assert" #-} do
    key <- AC.newKey
    vars <- H.new
    e <- runReaderT (buildB p) (AR ctx key vars)
    ST.assert ctx e
    H.toList vars

uservar :: (SolverAST ctx exp) => ctx -> FreeID -> Type -> AM ctx exp exp
uservar ctx id ty = do
  vars <- asks ar_vars
  liftIO $ do
    m <- H.lookup vars id
    case m of
        Nothing -> do
           declare ctx ty id
           H.insert vars id ty
        Just _ -> return ()
    var ctx id

buildB :: (SolverAST ctx exp) => BoolFF -> AM ctx exp exp
buildB x = do
   key <- asks ar_cachekey
   ctx <- asks ar_ctx
   case x of
     TrueFF -> liftIO $ bool ctx True
     FalseFF -> liftIO $ bool ctx False
     IteFF p a b c -> AC.cached c key $ do
        p' <- buildB p
        a' <- buildB a
        b' <- buildB b
        liftIO $ ite_bool ctx p' a' b'
     AndFF a b c -> AC.cached c key $ do
        a' <- buildB a
        b' <- buildB b
        liftIO $ and_bool ctx a' b'
     OrFF a b c -> AC.cached c key $ do
        a' <- buildB a
        b' <- buildB b
        liftIO $ or_bool ctx a' b'
     NotFF a c -> AC.cached c key $ do
        a' <- buildB a
        liftIO $ not_bool ctx a'
     VarFF id c -> AC.cached c key (uservar ctx id BoolT)
     Eq_IntegerFF a b c -> AC.cached c key $ do
        a' <- buildI a
        b' <- buildI b
        liftIO $ eq_integer ctx a' b'
     Leq_IntegerFF a b c -> AC.cached c key $ do
        a' <- buildI a
        b' <- buildI b
        liftIO $ leq_integer ctx a' b'
     Eq_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ eq_bit ctx a' b'
     Leq_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ leq_bit ctx a' b'

buildI :: (SolverAST ctx exp) => IntegerFF -> AM ctx exp exp
buildI x = do
   key <- asks ar_cachekey
   ctx <- asks ar_ctx
   case x of
     IntegerFF i -> liftIO $ integer ctx i
     Add_IntegerFF a b c -> AC.cached c key $ do
        a' <- buildI a
        b' <- buildI b
        liftIO $ add_integer ctx a' b'
     Sub_IntegerFF a b c -> AC.cached c key $ do
        a' <- buildI a
        b' <- buildI b
        liftIO $ sub_integer ctx a' b'
     Ite_IntegerFF p a b c -> AC.cached c key $ do
        p' <- buildB p
        a' <- buildI a
        b' <- buildI b
        liftIO $ ite_integer ctx p' a' b'
     Var_IntegerFF id c -> AC.cached c key (uservar ctx id IntegerT)

buildV :: (SolverAST ctx exp) => BitFF -> AM ctx exp exp
buildV x = do
  key <- asks ar_cachekey
  ctx <- asks ar_ctx
  case x of
    BitFF x -> liftIO $ bit ctx (bv_width x) (bv_value x)
    Add_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ add_bit ctx a' b'
    Sub_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ sub_bit ctx a' b'
    Mul_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ mul_bit ctx a' b'
    Or_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ or_bit ctx a' b'
    And_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ and_bit ctx a' b'
    Shl_BitFF w a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ shl_bit ctx w a' b'
    Lshr_BitFF w a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ lshr_bit ctx w a' b'
    Concat_BitFF a b c -> AC.cached c key $ do
        a' <- buildV a
        b' <- buildV b
        liftIO $ concat_bit ctx a' b'
    Not_BitFF a c -> AC.cached c key $ do
        a' <- buildV a
        liftIO $ not_bit ctx a'
    SignExtend_BitFF fr to a c -> AC.cached c key $ do
        a' <- buildV a
        liftIO $ sign_extend_bit ctx fr to a'
    Extract_BitFF hi lo a c -> AC.cached c key $ do
        a' <- buildV a
        liftIO $ extract_bit ctx hi lo a'
    Ite_BitFF p a b c -> AC.cached c key $ do
        p' <- buildB p
        a' <- buildV a
        b' <- buildV b
        liftIO $ ite_bit ctx p' a' b'
    Var_BitFF w id c -> AC.cached c key (uservar ctx id (BitT w))
