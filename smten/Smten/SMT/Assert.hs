
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
def ctx e
 | Just l <- de_litEH e = liftIO $ literal ctx l
 | Just p <- de_boolEH e = liftIO $ bool ctx p
 | VarEH (Sig nm _) <- force e = liftIO $ var ctx nm

 | PrimEH n t _ [a] <- force e
 , n == name "Smten.Bit.__prim_zeroExtend_Bit"  
 , Just sw <- de_bitT (typeof $ force a)
 , Just tw <- de_bitT t = do
    a' <- use a
    liftIO $ zeroextend ctx a' (tw - sw)

 | PrimEH n t _ [a] <- force e
 , n == name "Smten.Bit.__prim_signExtend_Bit"  
 , Just sw <- de_bitT (typeof $ force a)
 , Just tw <- de_bitT t = do
    a' <- use a
    liftIO $ signextend ctx a' (tw - sw)

 | PrimEH n t _ [x] <- force e
 , n == name "Smten.Bit.__prim_truncate_Bit"  
 , Just (_, bt) <- de_arrowT t
 , Just tw <- de_bitT bt = do
    x' <- use x
    liftIO $ AST.truncate ctx x' tw
 
 | PrimEH n t _ [x, li] <- force e
 , n == name "Smten.Bit.__prim_extract_Bit"  
 , Just i <- de_integerEH x
 , Just tw <- de_bitT t = do
    x' <- use x
    liftIO $ extract ctx x' (i+tw-1) i

 | PrimEH n t _ [a, b] <- force e = do
      case HT.lookup n binary of
        Just f -> do
            a' <- use a
            b' <- use b
            liftIO $ f ctx a' b'
        Nothing -> error $ "SMT binary primitive not supported: " ++ unname n
 | PrimEH n t _ [a] <- force e = do
      case HT.lookup n unary of
        Just f -> do
            a' <- use a
            liftIO $ f ctx a'
        Nothing -> error $ "SMT unary primitive not supported: " ++ unname n

 | IfEH _ p a b <- force e = do
     [p', a', b'] <- mapM use [p, a, b]
     liftIO $ ite ctx p' a' b'

