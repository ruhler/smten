
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Smten.Runtime.Assert (Smten.Runtime.Assert.assert) where

import qualified Data.HashTable.IO as H

import Smten.Runtime.AssertCache as AC
import Smten.Runtime.Bit
import Smten.Runtime.FreeID
import Smten.Runtime.Formula.Finite
import Smten.Runtime.Formula.Type
import Smten.Runtime.SolverAST as ST

type Vars = H.BasicHashTable FreeID Type

assert :: (SolverAST ctx exp) => ctx -> BoolFF -> IO [(FreeID, Type)]
assert ctx p = {-# SCC "Assert" #-} do
    key <- AC.newKey
    vars <- H.new :: IO Vars
    let uservar id ty = do
            m <- H.lookup vars id
            case m of
                Nothing -> do
                   declare ctx ty id
                   H.insert vars id ty
                Just _ -> return ()
            var ctx id

        buildB x = do
           case x of
             TrueFF -> bool ctx True
             FalseFF -> bool ctx False
             IteFF p a b c -> AC.cached c key $ do
                p' <- buildB p
                a' <- buildB a
                b' <- buildB b
                ite_bool ctx p' a' b'
             AndFF a b c -> AC.cached c key $ do
                a' <- buildB a
                b' <- buildB b
                and_bool ctx a' b'
             OrFF a b c -> AC.cached c key $ do
                a' <- buildB a
                b' <- buildB b
                or_bool ctx a' b'
             NotFF a c -> AC.cached c key $ do
                a' <- buildB a
                not_bool ctx a'
             VarFF id c -> AC.cached c key (uservar id BoolT)
             Eq_IntegerFF a b c -> AC.cached c key $ do
                a' <- buildI a
                b' <- buildI b
                eq_integer ctx a' b'
             Leq_IntegerFF a b c -> AC.cached c key $ do
                a' <- buildI a
                b' <- buildI b
                leq_integer ctx a' b'
             Eq_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                eq_bit ctx a' b'
             Leq_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                leq_bit ctx a' b'

        buildI x = do
           case x of
             IntegerFF i -> integer ctx i
             Add_IntegerFF a b c -> AC.cached c key $ do
                a' <- buildI a
                b' <- buildI b
                add_integer ctx a' b'
             Sub_IntegerFF a b c -> AC.cached c key $ do
                a' <- buildI a
                b' <- buildI b
                sub_integer ctx a' b'
             Ite_IntegerFF p a b c -> AC.cached c key $ do
                p' <- buildB p
                a' <- buildI a
                b' <- buildI b
                ite_integer ctx p' a' b'
             Var_IntegerFF id c -> AC.cached c key (uservar id IntegerT)

        buildV x = do
          case x of
            BitFF x -> bit ctx (bv_width x) (bv_value x)
            Add_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                add_bit ctx a' b'
            Sub_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                sub_bit ctx a' b'
            Mul_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                mul_bit ctx a' b'
            Or_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                or_bit ctx a' b'
            And_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                and_bit ctx a' b'
            Shl_BitFF w a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                shl_bit ctx w a' b'
            Lshr_BitFF w a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                lshr_bit ctx w a' b'
            Concat_BitFF a b c -> AC.cached c key $ do
                a' <- buildV a
                b' <- buildV b
                concat_bit ctx a' b'
            Not_BitFF a c -> AC.cached c key $ do
                a' <- buildV a
                not_bit ctx a'
            SignExtend_BitFF fr to a c -> AC.cached c key $ do
                a' <- buildV a
                sign_extend_bit ctx fr to a'
            Extract_BitFF hi lo a c -> AC.cached c key $ do
                a' <- buildV a
                extract_bit ctx hi lo a'
            Ite_BitFF p a b c -> AC.cached c key $ do
                p' <- buildB p
                a' <- buildV a
                b' <- buildV b
                ite_bit ctx p' a' b'
            Var_BitFF w id c -> AC.cached c key (uservar id (BitT w))
    e <- buildB p
    ST.assert ctx e
    H.toList vars

