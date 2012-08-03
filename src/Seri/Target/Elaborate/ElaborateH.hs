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

-- | Heap based approach for elaborating seri expressions.
-- Keeps track of sharing whereever possible.

module Seri.Target.Elaborate.ElaborateH (
    Mode(..), elaborate,
    ) where

import Control.Monad.ST

import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Simple elaboration does as little elaboration as possible once it's clear
-- progress can't be made.
--
-- Full elaboration fully elaborates everything.
-- A fully elaborated expression will be an infinite object if there is a
-- recursive call that can't be statically elaborated.
data Mode = Simple | Full
    deriving (Show, Eq)

data ES = ES {
    es_env :: Env,
    es_mode :: Mode,

    -- Declared variables already looked up
    es_decls :: Map.Map Sig ExpR,

    -- next available unique id to use
    es_nid :: ID
}

type ElabH s a = StateT ES (ST s) a

liftST :: ST s a -> ElabH s a
liftST = lift

type ID = Integer

-- | A reference to an expression.
-- This is annotated with 
--  ID - A unique identifier for the reference.
--  STRef - A pointer to the expression refered to
--  Set - The set of references refered to anywhere within the expression this
--  refers to. This is used during the deheapification phase only.
data ExpR s = ExpR ID (STRef s (ExpH, Maybe (Set.Set (ExpR s))))

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

data MatchH s = MatchH Pat (ExpR s)

heapify :: Exp -> ElabH ExpH
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

elabHed :: [Sig] -> ExpR -> ElabH ExpH
elabHed r = elabH free r >> readRef r

-- Elaborate the expression referred to by the given reference, given a list
-- of the free variables in the expression. By "free" here, we mean variables
-- which are in the expression which should not be looked up in the top level
-- declarations of the environment.
elabH :: [Sig] -> ExpR -> ElabH ()
elabH free r = do
  mode <- gets es_mode
  e <- readRef r
  case e of
    LitEH {} -> return ()
    CaseEH x ms -> do
       elabH free x
       mr <- matches free ms x
       case mr of
         -- TODO: return error if no alternative matches?
         NoMatched -> writeRef r $ CaseEH x [last ms]
         Matched vs b -> writeRef r (letEH vs b) >> elabH free r
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
                            LitEH lb -> binaryPrim n r la lb
                            _ -> return ()
                     _ -> return ()
          LamEH (Sig n _) body -> do
            p <- reducable n body
            if p
              then do
                writeRef r $ reduce s b body
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
            Just x -> writeRef r x >> elabH free r
            Nothing ->
              case (attemptM $ lookupVar env s) of
                 Nothing -> return ()
                 Just (pt, ve) -> do 
                    er <- mkRef $ HeapifyEH (assign (assignments pt ct) ve)
                    modify $ \es -> es { es_decls = Map.insert s er decls }
                    writeRef r (RefEH er)
                    elabH free r
    RefEH x ->
      xv <- elabHed free x
      case xv of
        RefEH y -> writeRef r xv
        _ -> return ()
    HeapifyEH exp ->
        heapified <- heapify exp
        writeRef r heapified
        elabH free r

-- | Return true if there is an occurence of the given signature free in the
-- given expression. In other words, say if beta reduction on the given
-- variable would do anything or not.
--
-- TODO: I expect this to be a slow operation to do over and over and over and
-- over again. We can reduce the complexity if we build up a cache of the
-- results at each intermediate expression first, then just traverse from that
-- cache and read the answer.
reducable :: Name -> ExpR -> ElabH Bool
reducable n e =
  case e of
    LitEH {} -> False
    CaseEH x ms ->
      let rm (MatchH p _) | n `elem` bindingsP' p = return False
          rm (MatchH _ b) = reducable n b
      in do
        rx <- reducable n x
        rms <- mapM rm ms
        return $ or (rx:rms)
    AppEH a b ->
        ra <- reducable a
        rb <- reducable b
        return $ ra || rb
    LamEH (Sig nm _) _ | nm == n -> return False
    LamEH _ b -> reducable n b
    ConEH {} -> return False
    VarEH (Sig nm _) | nm == n -> return True
    VarEH {} -> return False
    RefEH r -> reducable n r
    HeapifyEH exp -> return $ reducableE n exp

-- | Reducable predicate for pure seri expressions.
reducableE :: Name -> Exp -> Bool
reducableE n exp = n `elem` [nm | Sig nm _ <- free exp]

reduceEH :: Sig -> ExpR -> ExpR -> ExpH
reduceEH s v b = AppEH (LamEH s b) v
            
-- reduce n v e
-- Replace occurences of n with v in the expression e.
-- Returns a reference to the new expression with replacements.
reduce :: Sig -> ExpR -> ExpR -> ElabH ExpR
reduce s@(Sig n _) v r = do
  e <- readRef r
  case e of
    LitEH {} -> return r
    CaseEH x ms ->
      let rm :: MatchH -> ElabH MatchH
          rm (MatchH p _) | n `elem` bindingsP' p = return r
          rm (MatchH p b) = do
            b' <- mkRef $ reduceEH s v b
            return (MatchH p b')
      in do
        x' <- mkRef $ reduceEH s v x
        ms' <- mapM rm ms 
        mkRef $ CaseEH x' ms'
    AppEH a b -> do
        a' <- mkRef $ reduceEH s v a
        b' <- mkRef $ reduceEH s v b
        mkRef $ AppEH a' b'
    LamEH (Sig nm _) _ | nm == n -> return r
    LamEH ls b -> do
        b' <- mkRef $ reduceEH s v b
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
mkRef :: ExpH s -> ElabH (ExpR s)
mkRef e = do
    id <- gets es_nid
    modify $ \es -> es { es_nid = id+1 }
    r <- liftST $ newSTRef (e, Nothing)
    return $ ExpR id r


-- | Read a reference.
readRef1 :: ExpR -> ElabH ExpH
readRef1 r = fmap fst $ liftST $ readSTRef r

-- | Read a reference, following chains to the end.
readRef :: ExpR -> ElabH ExpH
readRef r = do
  v <- readRef1 
  case v of
    RefEH x -> readRef x
    _ -> return v

writeRef :: ExpR -> ExpH -> ElabH ()
writeRef r e = liftST $ writeSTRef r (e, Nothing)
    
readReachable :: ExpR -> ElabH (Maybe (Set.Set ExpR))
readReachable r = fmap snd $ liftST $ readSTRef r

isBinaryPrim :: Name -> Bool
isBinaryPrim = n `elem` [
   "Seri.Lib.Prelude.__prim_eq_Char",
   "Seri.Lib.Prelude.__prim_add_Integer",
   "Seri.Lib.Prelude.__prim_sub_Integer",
   "Seri.Lib.Prelude.__prim_mul_Integer",
   "Seri.Lib.Prelude.<",
   "Seri.Lib.Prelude.>",
   "Seri.Lib.Prelude.__prim_eq_Integer"]

binaryPrim :: Name -> Lit -> Lit -> ExpR -> ElabH ()
binaryPrim "Seri.Lib.Prelude.__prim_eq_Char" (CharL a) (CharL b) r = writeRef r $ boolEH (a == b)
binaryPrim "Seri.Lib.Prelude.__prim_add_Integer" (InteralL a) (IntegerL b) r = writeRef r $ integerEH (a + b)
binaryPrim "Seri.Lib.Prelude.__prim_sub_Integer" (InteralL a) (IntegerL b) r = writeRef r $ integerEH (a - b)
binaryPrim "Seri.Lib.Prelude.__prim_mul_Integer" (InteralL a) (IntegerL b) r = writeRef r $ integerEH (a * b)
binaryPrim "Seri.Lib.Prelude.<" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a < b)
binaryPrim "Seri.Lib.Prelude.>" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a > b)
binaryPrim "Seri.Lib.Prelude.__prim_eq_Integer" (IntegerL a) (IntegerL b) r = writeRef r $ boolEH (a == b)
binaryPrim _ _ _ _ = return ()

        
class Reachable a where
  -- Return the set of complex references reachable from the given object.
  -- Also updates the cache of reachable objects.
  reachable :: a -> ELabH (Set.Set (ExpR s))

instance Reachable ExpH where
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

instance Reachable MatchH where
  reachable (MatchH p b) = reachable b

instance Reachable ExpR where
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
deheapify :: Set.Set ExpR -> ExpH -> ElabH Exp
deheapify f e =
  case e of
    LitEH l -> return (LitE l)
    CaseEH x ms -> do
        rx <- reachable x
        rms <- reachable ms
        let shared = Set.intersection rx rms
        let dohere = Set.difference shared f
        bindings <- mapM mkBinding f (elems dohere)
        xdh <- deheapify (Set.intersection rx shared) x
        msdh <- deheapify (Set.intersection rms shared) ms
        return $ letE bindings (CaseE xdh mdsh)
    AppEH a b -> do
        ra <- reachable a
        rb <- reachable b
        let shared = Set.intersection ra rb
        let dohere = Set.difference shared f
        bindings <- mapM mkBinding f (elems dohere)
        adh <- deheapify (Set.intersection ra shared) a
        bdh <- deheapify (Set.intersection rb shared) b
        return $ letE bindings (AppE adh bdh)
    LamEH s b -> do
        bdh <- deheapify f b
        return $ LamE s bdh
    ConEH s -> return (ConE s)
    VarEH s -> return (VarE s)
    RefEH x -> do
      v <- readRef x
      deheapify f v
    HeapifyEH exp -> return exp

mkBinding :: Set.Set ExpR -> ExpR -> ElabH (Sig, Exp)
mkBinding f r@(ExpR id _) = do
   v <- readRef r
   e <- deheapify f v
   return (Sig ("~" ++ id) (typeof e), e)

data MatchesResult
 = Matched [(Sig, ExpH)] ExpH
 | UnMatched [MatchH]
 | NoMatched

-- Match an expression against a sequence of alternatives.
-- The expression should be elaborated already.
matches :: [Name] -> ExpR -> [MatchH] -> ElabH MatchesResult
matches _ _ [] = return NoMatched
matches free x ms@((MatchH p b):_) = do
  r <- match free p x of
  case r of 
    Failed -> matches free x (tail ms)
    Succeeded vs -> MatchedH vs b
    Unknown -> UnMatched ms

data MatchResult = Failed | Succeeded [(Sig, ExpR)] | Unknown

match :: [Name] -> Pat -> ExpR -> ElabH MatchResult
match free p r = do
  v <- readRef free r
  case p of
    ConP _ nm [] ->
      case v of
        ConEH (Sig n _) | n == nm -> return $ Succeeded []
        _ | iswhnf v -> return Failed
        _ -> return Unknown
    ConP t n ps ->
      case v of
        (AppEH ae be) -> do
          ma <- match (ConP t n (init ps)) ae
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
          _ | iswhnf v -> return Failed
          _ -> return Unknown
    IntegerP i ->
      case v of
        LitEH (IntegerL i') | i == i' -> return $ Succeeded []
        _ | iswhnf v -> return Failed
        _ -> return Unknown
    VarP s -> return $ Succeeded [(s, r)]
    WildP {} -> return $ Succeeded []
    

-- iswhnf exp
--  Return True if the expression is in weak head normal form.
--  TODO: how should we handle primitives?
iswhnf :: ExpH -> Bool
iswhnf (LitEH {}) = True
iswhnf (LamEH {}) = True
iswhnf x
 = let iscon :: ExpH -> Bool
       iscon (ConEH {}) = True
       iscon (AppEH f _) = iscon f
       iscon _ = False
   in iscon x

elaborateH :: Exp -> ElabH Exp
elaborateH e = do
    heapified <- heapify e
    elabH [] heapified
    v <- readRef heapified
    deheapify Set.empty v

elaborateST :: Mode -> Env -> Exp -> ST s Exp
elaborateST mode env e = evalStateT (elaborateH e) (ES env mode Map.empty 1)

elaborate :: Mode -> Env -> Exp -> Exp
elaborate mode env e = runST $ elaborateST mode env e

