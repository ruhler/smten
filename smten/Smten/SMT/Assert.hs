
{-# LANGUAGE PatternGuards #-}

module Smten.SMT.Assert (Smten.SMT.Assert.assert) where

import Control.Monad.State
import qualified Data.HashMap as Map

import qualified Smten.HashTable as HT
import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.ExpH
import Smten.SMT.AST as AST

data AS ctx exp = AS {
    as_ctx :: ctx,
    as_exps :: Map.Map EID exp
}

type AssertM ctx exp = StateT (AS ctx exp) IO

assert :: (AST ctx exp) => ctx -> ExpH -> IO ()
assert ctx p = do
    e <- evalStateT (def ctx p) (AS ctx Map.empty)
    AST.assert ctx e

use :: (AST ctx exp) => ExpH -> AssertM ctx exp exp
use e = do
  m <- gets as_exps
  case Map.lookup (eid e) m of
      Just v -> return v
      Nothing -> do
        ctx <- gets as_ctx
        v <- def ctx e
        modify $ \s -> s { as_exps = Map.insert (eid e) v (as_exps s) } 
        return v

def :: (AST ctx exp) => ctx -> ExpH -> AssertM ctx exp exp
def ctx e =
  case force e of
    LitEH l -> liftIO $ literal ctx l
    ConEH n [] -> liftIO $ bool ctx (n == trueN)
    VarEH nm -> liftIO $ var ctx nm

    PrimEH n _ [a, b]
      | n == name "Smten.Bit.__prim_extract_Bit"  
      , Just i <- de_integerEH b
      , Just tw <- de_bitT (typeof e) -> do
            a' <- use a
            liftIO $ extract ctx a' (i+tw-1) i

      | otherwise -> do
          case HT.lookup n binary of
            Just f -> do
                a' <- use a
                b' <- use b
                liftIO $ f ctx a' b'
            Nothing -> error $ "SMT binary primitive not supported: " ++ unname n

    PrimEH n _ [a] 
      | n == name "Smten.Bit.__prim_zeroExtend_Bit"  
      , Just sw <- de_bitT (typeof a)
      , Just tw <- de_bitT (typeof e) -> do
           a' <- use a
           liftIO $ zeroextend ctx a' (tw - sw)

      | n == name "Smten.Bit.__prim_signExtend_Bit"  
      , Just sw <- de_bitT (typeof a)
      , Just tw <- de_bitT (typeof e) -> do
           a' <- use a
           liftIO $ signextend ctx a' (tw - sw)

      | otherwise -> do
          case HT.lookup n unary of
            Just f -> do
                a' <- use a
                liftIO $ f ctx a'
            Nothing -> error $ "SMT unary primitive not supported: " ++ unname n

    IfEH p a b -> do
       [p', a', b'] <- mapM use [p, a, b]
       liftIO $ ite ctx p' a' b'

    -- abstract away explicit errors with a variable.
    -- SMT.SMT will verify we don't actually reach this error under the
    -- satisfying assignment.
    ErrorEH _ -> liftIO $ do
       nm <- fresh ctx (typeof e)
       var ctx nm

