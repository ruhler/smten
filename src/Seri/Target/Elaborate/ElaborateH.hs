-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Heap based approach for elaborating seri expressions.
-- Keeps track of sharing whereever possible.

module Seri.Target.Elaborate.ElaborateH (
    Mode(..), elaborate,
    ) where

import Prelude hiding (print)

import Debug.Trace

import Control.Monad.ST
import Control.Monad.State

import Data.Monoid
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Seri.Failable
import Seri.Lambda

-- | Simple elaboration does as little elaboration as possible once it's clear
-- progress can't be made.
--
-- Full elaboration fully elaborates everything.
-- A fully elaborated expression will be an infinite object if there is a
-- recursive call that can't be statically elaborated.
data Mode = Simple | Full
    deriving (Show, Eq)

data ES s = ES {
    es_env :: Env,
    es_mode :: Mode,

    -- Declared variables already looked up
    es_decls :: Map.Map Sig (ExpR s),

    -- next available unique id to use
    es_nid :: ID
}

type ElabH s a = StateT (ES s) (ST s) a

liftST :: ST s a -> ElabH s a
liftST = lift

type ID = Integer

-- | A reference to an expression.
-- This is annotated with 
--  ID - A unique identifier for the reference.
--  STRef - A pointer to the expression refered to
--  Set - The set of references refered to anywhere within the expression this
--  refers to. This is used during the deheapification phase only.
data ExpR s = ExpR ID (STRef s (ExpH s, Maybe (Set.Set (ExpR s))))

instance Eq (ExpR s) where
    (==) (ExpR a _) (ExpR b _) = (a == b)

instance Ord (ExpR s) where
    (<=) (ExpR a _) (ExpR b _) = a <= b

-- | Heapify is made explicitly lazy.
-- This is so we can very efficiently deheapify an unelaborated heapified
-- expression.
--
-- deheapify (HeapifyEH e) ==> e
data ExpH s
  = LitEH Lit
  | CaseEH (ExpR s) [MatchH s]
  | AppEH (ExpR s) (ExpR s)
  | LamEH Sig (ExpR s)
  | ConEH Sig
  | VarEH Sig
  | RefEH (ExpR s)
  | HeapifyEH Exp

printr :: ExpR s -> String
printr (ExpR id _) = "~" ++ show id

print :: ExpH s -> String
print e = 
  let printms [] = ""
      printms (m:ms) = printm m ++ "; " ++ printms ms
      printm (MatchH p b) = pretty p ++ " -> " ++ printr b
  in case e of
       LitEH l -> pretty l
       CaseEH r ms -> "CaseEH " ++ printr r ++ " " ++ printms ms
       AppEH a b -> "AppEH " ++ printr a ++ " " ++ printr b 
       LamEH s b -> "LamEH " ++ pretty s ++ " " ++ printr b
       ConEH s -> "ConEH " ++ pretty s
       VarEH s -> "VarEH " ++ pretty s
       RefEH r -> "RefEH " ++ printr r
       HeapifyEH e -> "HeapifyEH " ++ pretty e

    
data MatchH s = MatchH Pat (ExpR s)

heapify :: Exp -> ElabH s (ExpH s)
heapify e =
  case e of
    LitE l -> return $ LitEH l
    CaseE x ms ->
      let hm (Match p b) = do
             b' <- mkRef (HeapifyEH b)
             return (MatchH p b')
      in do
         x' <- mkRef (HeapifyEH x)
         ms' <- mapM hm ms
         return $ CaseEH x' ms'
    AppE a b -> do
        a' <- mkRef (HeapifyEH a)
        b' <- mkRef (HeapifyEH b)
        return (AppEH a' b')
    LamE s b -> do
        b' <- mkRef (HeapifyEH b)
        return (LamEH s b')
    VarE s -> return (VarEH s)
    ConE s -> return (ConEH s)

elabHed :: [Sig] -> ExpR s -> ElabH s (ExpH s)
elabHed free r = elabH free r >> readRef r

-- Elaborate the expression referred to by the given reference, given a list
-- of the free variables in the expression. By "free" here, we mean variables
-- which are in the expression which should not be looked up in the top level
-- declarations of the environment.
elabH :: [Sig] -> ExpR s -> ElabH s ()
elabH free r = do
  mode <- gets es_mode
  e <- readRef r
  --case (trace ("elab " ++ printr r) e) of
  case e of
    LitEH {} -> return ()
    CaseEH x ms -> do
       elabH free x
       mr <- matches free x ms
       case mr of
         -- TODO: return error if no alternative matches?
         NoMatched -> writeRef r $ CaseEH x [last ms]
         Matched vs b -> do
            lete <- letEH vs b
            writeRef r (RefEH lete)
            elabH free r
         UnMatched ms' | mode == Simple -> writeRef r $ CaseEH x ms'
         UnMatched ms' | mode == Full -> 
            let elabmH (MatchH p b) = elabH (bindingsP p ++ free) b
            in mapM_ elabmH ms'
    AppEH a b -> do
        av <- elabHed free a
        case av of
          VarEH (Sig "Seri.Lib.Prelude.valueof" t) ->
             let NumT nt = head $ unarrowsT t
             in writeRef r $ integerEH (nteval nt)
          AppEH p x ->  do -- Handle binary primitives
             pv <- readRef p
             case pv of
                VarEH (Sig n _) | isBinaryPrim n -> do
                   xv <- elabHed free x
                   case xv of
                     LitEH la -> do
                        bv <- elabHed free b
                        case bv of
                            LitEH lb -> binaryPrim n la lb r
                            _ -> return ()
                     _ | mode == Full -> elabH free b
                     _ -> return ()
                _ -> return ()
          LamEH s@(Sig n _) body -> do
            rr <- reduce s b body 
            writeRef r $ RefEH rr
            elabH free r

          _ | mode == Full -> elabH free b
          _ -> return ()
    LamEH {} | mode == Simple -> return ()
    LamEH s b -> elabH (s:free) b
    ConEH {} -> return ()
    VarEH (Sig "Seri.Lib.Prelude.numeric" (NumT nt)) ->
        writeRef r $ ConEH (Sig ("#" ++ show (nteval nt)) (NumT nt))
    VarEH s | s `elem` free -> return ()
    VarEH s@(Sig _ ct) -> do
        env <- gets es_env
        decls <- gets es_decls
        case Map.lookup s decls of
            Just x -> writeRef r (RefEH x) >> elabH free r
            Nothing ->
              case (attemptM $ lookupVar env s) of
                 Nothing -> return ()
                 Just (pt, ve) -> do 
                    er <- mkRef $ HeapifyEH (assign (assignments pt ct) ve)
                    modify $ \es -> es { es_decls = Map.insert s er decls }
                    writeRef r (RefEH er)
                    elabH free r
    RefEH x -> do
      xv <- elabHed free x
      case xv of
        RefEH y -> writeRef r xv
        _ -> return ()
    HeapifyEH exp -> do
        heapified <- heapify exp
        writeRef r heapified
        elabH free r

-- reduce n v e
-- Replace occurences of n with v in the expression e.
-- Returns a reference to the new expression with replacements.
--
-- TODO: if the reduced expression is the same as the original, we could just
-- return the original to capture more sharing.
reduce :: Sig -> ExpR s -> ExpR s -> ElabH s (ExpR s)
reduce s@(Sig n _) v r = do
  e <- readRef r
  case e of
    LitEH {} -> return r
    CaseEH x ms ->
      let rm m@(MatchH p _) | n `elem` bindingsP' p = return m
          rm (MatchH p b) = do
            b' <- reduce s v b
            return (MatchH p b')
      in do
        x' <- reduce s v x
        ms' <- mapM rm ms 
        mkRef $ CaseEH x' ms'
    AppEH a b -> do
        a' <- reduce s v a
        b' <- reduce s v b
        mkRef $ AppEH a' b'
    LamEH (Sig nm _) _ | nm == n -> return r
    LamEH ls b -> do
        b' <- reduce s v b
        mkRef $ LamEH ls b'
    ConEH {} -> return r
    VarEH (Sig nm _) | nm == n -> return v
    VarEH {} -> return r
    RefEH x -> reduce s v x
    HeapifyEH exp -> do
        x <- heapify exp
        writeRef r x
        reduce s v r

-- | Make and return a reference to the given expression.
mkRef :: ExpH s -> ElabH s (ExpR s)
mkRef e = do
    id <- gets es_nid
    modify $ \es -> es { es_nid = id+1 }
    r <- liftST $ newSTRef (e, Nothing)
    let er = ExpR id r
    --trace (printr er ++ ": " ++ print e) (return er)
    return er


-- | Read a reference.
readRef1 :: ExpR s -> ElabH s (ExpH s)
readRef1 (ExpR _ r) = fmap fst $ liftST $ readSTRef r

-- | Read a reference, following chains to the end.
readRef :: ExpR s -> ElabH s (ExpH s)
readRef r = do
  v <- readRef1 r
  case v of
    RefEH x -> readRef x
    _ -> return v

writeRef :: ExpR s -> (ExpH s) -> ElabH s ()
writeRef er@(ExpR id r) e = --trace (printr er ++ ": " ++ print e) $
    liftST $ writeSTRef r (e, Nothing)
    
readReachable :: ExpR s -> ElabH s (Maybe (Set.Set (ExpR s)))
readReachable (ExpR _ r) = fmap snd $ liftST $ readSTRef r

writeReachable :: ExpR s -> Set.Set (ExpR s) -> ElabH s ()
writeReachable (ExpR _ r) s = liftST $ do
  (e, _) <- readSTRef r
  writeSTRef r (e, Just s)

isBinaryPrim :: Name -> Bool
isBinaryPrim n = n `elem` [
   "Seri.Lib.Prelude.__prim_eq_Char",
   "Seri.Lib.Prelude.__prim_add_Integer",
   "Seri.Lib.Prelude.__prim_sub_Integer",
   "Seri.Lib.Prelude.__prim_mul_Integer",
   "Seri.Lib.Prelude.<",
   "Seri.Lib.Prelude.>",
   "Seri.Lib.Prelude.__prim_eq_Integer"]

binaryPrim :: Name -> Lit -> Lit -> ExpR s -> ElabH s ()
binaryPrim "Seri.Lib.Prelude.__prim_eq_Char" (CharL a) (CharL b) r = writeRef r $ boolEH (a == b)
binaryPrim "Seri.Lib.Prelude.__prim_add_Integer" (IntegerL a) (IntegerL b) r = writeRef r $ integerEH (a + b)
binaryPrim "Seri.Lib.Prelude.__prim_sub_Integer" (IntegerL a) (IntegerL b) r = writeRef r $ integerEH (a - b)
binaryPrim "Seri.Lib.Prelude.__prim_mul_Integer" (IntegerL a) (IntegerL b) r = writeRef r $ integerEH (a * b)
binaryPrim "Seri.Lib.Prelude.<" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a < b)
binaryPrim "Seri.Lib.Prelude.>" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a > b)
binaryPrim "Seri.Lib.Prelude.__prim_eq_Integer" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a == b)
binaryPrim _ _ _ _ = return ()

        
class Reachable a s where
  -- Return the set of complex references reachable from the given object.
  -- Also updates the cache of reachable objects.
  reachable :: a -> ElabH s (Set.Set (ExpR s))

instance (Reachable a s) => Reachable [a] s where 
  reachable xs = fmap Set.unions $ mapM reachable xs

instance Reachable (ExpH s) s where
  reachable e = 
    case e of 
        LitEH {} -> return Set.empty
        CaseEH x ms -> do
            rx <- reachable x
            rms <- reachable ms
            return (Set.union rx rms)
        AppEH a b -> do
            ra <- reachable a
            rb <- reachable b
            return (Set.union ra rb)
        LamEH s b -> reachable b
        ConEH {} -> return Set.empty
        VarEH {} -> return Set.empty
        RefEH x -> reachable x
        HeapifyEH {} -> return Set.empty

instance Reachable (MatchH s) s where
  reachable (MatchH p b) = reachable b

instance Reachable (ExpR s) s where
  reachable r =
    let isComplex (CaseEH {}) = True
        isComplex (AppEH {}) = True
        isComplex (LamEH {}) = True
        isComplex (HeapifyEH {}) = True
        isComplex _ = False
    in do
        mrs <- readReachable r
        case mrs of
          Just rs -> return rs
          Nothing -> do
            v <- readRef r
            rs <- reachable v
            let rs' = if isComplex v then Set.insert r rs else rs
            writeReachable r rs'
            return rs'

-- Given the set of references which can be assumed to be in scope, deheapify
-- an expression.
deheapify :: Set.Set (ExpR s) -> (ExpR s) -> ElabH s Exp
deheapify f r = do
  e <- readRef r 
  case e of
    LitEH l -> return (LitE l)
    CaseEH x ms -> do
        rx <- reachable x
        rms <- reachable ms
        let shared = Set.intersection rx rms
        let dohere = Set.difference shared f
        bindings <- mapM (mkBinding f) (Set.elems dohere)
        xdh <- deheapify (Set.intersection rx shared) x
        msdh <- mapM (deheapifym (Set.intersection rms shared)) ms
        return $ letE bindings (CaseE xdh msdh)
    AppEH a b -> do
        ra <- reachable a
        rb <- reachable b
        let shared = Set.intersection ra rb
        let dohere = Set.difference shared f
        bindings <- mapM (mkBinding f) (Set.elems dohere)
        adh <- deheapify (Set.intersection ra shared) a
        bdh <- deheapify (Set.intersection rb shared) b
        return $ letE bindings (AppE adh bdh)
    LamEH s b -> do
        bdh <- deheapify f b
        return $ LamE s bdh
    ConEH s -> return (ConE s)
    VarEH s -> return (VarE s)
    RefEH x -> deheapify f x
    HeapifyEH exp -> return exp

deheapifym :: Set.Set (ExpR s) -> MatchH s -> ElabH s Match
deheapifym f (MatchH p b) = do
    bdh <- deheapify f b
    return (Match p bdh)
    

mkBinding :: Set.Set (ExpR s) -> ExpR s -> ElabH s (Sig, Exp)
mkBinding f r@(ExpR id _) = do
   e <- deheapify f r
   return (Sig ("~" ++ show id) (typeof e), e)

data MatchesResult s
 = Matched [(Sig, ExpR s)] (ExpR s)
 | UnMatched [MatchH s]
 | NoMatched

-- Match an expression against a sequence of alternatives.
-- The expression should be elaborated already.
matches :: [Sig] -> (ExpR s) -> [MatchH s] -> ElabH s (MatchesResult s)
matches _ _ [] = return NoMatched
matches free x ms@((MatchH p b):_) = do
  r <- match free p x
  case r of 
    Failed -> matches free x (tail ms)
    Succeeded vs -> return $ Matched vs b
    Unknown -> return $ UnMatched ms

data MatchResult s = Failed | Succeeded [(Sig, ExpR s)] | Unknown

match :: [Sig] -> Pat -> ExpR s -> ElabH s (MatchResult s)
match free p r = do
  v <- readRef r
  case p of
    ConP _ nm [] ->
      case v of
        ConEH (Sig n _) | n == nm -> return $ Succeeded []
        _ -> do
          whnf <- iswhnf v
          return $ if whnf then Failed else Unknown
    ConP t n ps ->
      case v of
        (AppEH ae be) -> do
          ma <- match free (ConP t n (init ps)) ae
          case ma of
            Failed -> return Failed
            Unknown -> return Unknown
            Succeeded as -> do
              elabH free be
              mb <- match free (last ps) be
              case mb of
                Succeeded bs -> return $ Succeeded (as ++ bs)
                Failed -> return Failed
                Unknown -> return Unknown
        _ -> do
          whnf <- iswhnf v
          return $ if whnf then Failed else Unknown
    IntegerP i ->
      case v of
        LitEH (IntegerL i') | i == i' -> return $ Succeeded []
        _ -> do
          whnf <- iswhnf v
          return $ if whnf then Failed else Unknown
    VarP s -> return $ Succeeded [(s, r)]
    WildP {} -> return $ Succeeded []
    

-- iswhnf exp
--  Return True if the expression is in weak head normal form.
--  TODO: how should we handle primitives?
iswhnf :: ExpH s -> ElabH s Bool
iswhnf (LitEH {}) = return True
iswhnf (LamEH {}) = return True
iswhnf x
 = let iscon (ConEH {}) = return True
       iscon (AppEH f _) = readRef f >>= iscon
       iscon _ = return False
   in iscon x

elaborateH :: Exp -> ElabH s Exp
elaborateH e = do
    r <- mkRef (HeapifyEH e)
    elabH [] r
    deheapify Set.empty r

elaborateST :: Mode -> Env -> Exp -> ST s Exp
elaborateST mode env e = evalStateT (elaborateH e) (ES env mode Map.empty 1)

elaborate :: Mode -> Env -> Exp -> Exp
elaborate mode env e =
  runST $ elaborateST mode env e
  --let elabed = runST $ elaborateST mode env (trace ("elab: " ++ pretty e) e)
  --in trace ("elabed: " ++ pretty elabed) elabed

letEH :: [(Sig, ExpR s)] -> ExpR s -> ElabH s (ExpR s)
letEH [] x = return x
letEH ((s, v):bs) x = do
    sub <- letEH bs x
    r <- mkRef $ LamEH s sub
    mkRef (AppEH r v)

integerEH :: Integer -> ExpH s
integerEH i = LitEH (IntegerL i)

-- | True
trueEH :: ExpH s
trueEH = ConEH (Sig "True" (ConT "Bool"))

-- | False
falseEH :: ExpH s
falseEH = ConEH (Sig "False" (ConT "Bool"))

-- | Boolean expression
boolEH :: Bool -> ExpH s
boolEH True = trueEH
boolEH False = falseEH

