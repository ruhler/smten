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

import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set

import Seri.Failable
import Seri.Lambda

data Mode =
      WHNF      -- ^ Reduce to weak head normal form
    | SNF       -- ^ Reduce to sharing normal form
    deriving (Show, Eq)

data ES s = ES {
    es_env :: Env,
    es_mode :: Mode,

    -- Declared variables already looked up
    es_decls :: Map.Map Sig (ExpR s),

    -- next available unique id to use
    es_nid :: ID,

    -- unique name map. Maps name to an integer which can be appended to it to
    -- make a unique name.
    es_unique :: Map.Map Name Integer
}

type ElabH s = StateT (ES s) (ST s)

liftST :: ST s a -> ElabH s a
liftST = lift

newname :: Name -> ElabH s Name
newname n = do
    m <- gets es_unique
    let (id, m') = Map.insertLookupWithKey (\_ -> (+)) n 1 m
    modify $ \es -> es { es_unique = m' }
    return $ n ++ "~" ++ show (fromMaybe 0 id)

type ID = Integer

-- | A reference to an expression.
-- This is annotated with 
--  ID - A unique identifier for the reference.
--  Type - the type of thing the reference points to.
--  STRef - A pointer to the expression refered to by the reference.
data ExpR s = ExpR ID Type (STRef s (ExpH s))

instance Eq (ExpR s) where
    (==) (ExpR a _ _) (ExpR b _ _) = (a == b)

instance Ord (ExpR s) where
    (<=) (ExpR a _ _) (ExpR b _ _) = a <= b

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
printr (ExpR id _ _) = "~" ++ show id

-- Variable name to use for the given reference in the deheapified expression.
rname :: ExpR s -> Name
rname = printr

rtype :: ExpR s -> Type
rtype r@(ExpR _ t _) = t

rsig :: ExpR s -> Sig
rsig r = Sig (rname r) (rtype r)

rvar :: ExpR s -> Exp
rvar r = VarE (rsig r)

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

-- | Heapify an expression, given a rename mapping for bound variables.
heapify :: Map.Map Name Name -> Exp -> ElabH s (ExpH s)
heapify m e =
  case e of
    LitE l -> return $ LitEH l
    CaseE x ms ->
      let repat :: Map.Map Name Name -> Pat -> Pat 
          repat m (ConP t n ps) = ConP t n (map (repat m) ps)
          repat m p@(VarP (Sig n t)) =
            case Map.lookup n m of
                Just n' -> VarP (Sig n' t)
                Nothing -> p
          repat m p@(IntegerP {}) = p
          repat m p@(WildP {}) = p

          hm (Match p b) = do
             let nms = bindingsP' p
             nms' <- mapM newname nms
             let m' = Map.union (Map.fromList (zip nms nms')) m
             b' <- heapifyr m' b
             return (MatchH (repat m' p) b')
      in do
         x' <- heapifyr m x
         ms' <- mapM hm ms
         return $ CaseEH x' ms'
    AppE a b -> do
        a' <- heapifyr m a
        b' <- heapifyr m b
        return (AppEH a' b')
    LamE (Sig n t) b -> do
        n' <- newname n
        let m' = Map.insert n n' m
        b' <- heapifyr m' b
        return (LamEH (Sig n' t) b')
    VarE s@(Sig n t) ->
        case Map.lookup n m of
            Just n' -> return (VarEH (Sig n' t))
            Nothing -> return (VarEH s)
    ConE s -> return (ConEH s)

-- Same as heapify, but makes a new reference to the expression and returns
-- that.
heapifyr :: Map.Map Name Name -> Exp -> ElabH s (ExpR s)
heapifyr m e = do
    e' <- heapify m e
    mkRef (typeof e) e'

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
         NoMatched -> error $ "Case did not match"
         Matched vs b -> do
            lete <- letEH vs b
            writeRef r (RefEH lete)
            elabH free r
         UnMatched ms' -> do
            writeRef r $ CaseEH x ms'
            if mode == SNF
                then sequence_ [elabH (bindingsP p ++ free) b | MatchH p b <- ms']
                else return ()
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
                     _ | mode == SNF -> elabH free b
                     _ -> return ()
                _ | mode == SNF -> elabH free b
                _ -> return ()
          LamEH s@(Sig n _) body -> do
            rr <- reduce s b body 
            writeRef r $ RefEH rr
            elabH free r

          _ | mode == SNF -> elabH free b
          _ -> return ()
    LamEH {} | mode == WHNF -> return ()
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
                    let ve' = (assign (assignments pt ct) ve)
                    er <- mkRef (typeof ve') $ HeapifyEH ve'
                    modify $ \es -> es { es_decls = Map.insert s er decls }
                    writeRef r (RefEH er)
                    elabH free r
    RefEH x -> error $ "readRef returned RefEH in elabH"
    HeapifyEH exp -> do
        heapified <- heapify Map.empty exp
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
        mkRef (rtype r) $ CaseEH x' ms'
    AppEH a b -> do
        a' <- reduce s v a
        b' <- reduce s v b
        mkRef (rtype r) $ AppEH a' b'
    LamEH (Sig nm _) _ | nm == n -> return r
    LamEH ls b -> do
        b' <- reduce s v b
        mkRef (rtype r) $ LamEH ls b'
    ConEH {} -> return r
    VarEH (Sig nm _) | nm == n -> return v
    VarEH {} -> return r
    RefEH x -> error $ "readRef returned RefEH in reduce"
    HeapifyEH exp -> do
        x <- heapify Map.empty exp
        writeRef r x
        reduce s v r

-- | Make and return a reference to the given expression.
mkRef :: Type -> ExpH s -> ElabH s (ExpR s)
mkRef t e = do
    id <- gets es_nid
    modify $ \es -> es { es_nid = id+1 }
    r <- liftST $ newSTRef e
    let er = ExpR id t r
    --trace (printr er ++ ": " ++ print e) (return er)
    return er


-- | Read a reference.
readRef1 :: ExpR s -> ElabH s (ExpH s)
readRef1 (ExpR _ _ r) = liftST $ readSTRef r

-- | Read a reference, following chains to the end.
readRef :: ExpR s -> ElabH s (ExpH s)
readRef r = do
  v <- readRef1 r
  case v of
    RefEH x -> readRef x
    _ -> return v

writeRef :: ExpR s -> (ExpH s) -> ElabH s ()
writeRef er@(ExpR _ _ r) e = --trace (printr er ++ ": " ++ print e) $
    liftST $ writeSTRef r e

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

isfunt :: Type -> Bool
isfunt t = 
  case unarrowsT t of
      _:_:_ -> True
      _ -> False
        
-- Given the set of references which can be assumed to be in scope, deheapify
-- an expression, sharing complex expressions wherever possible.
deheapify :: ExpR s -> ElabH s Exp
deheapify r =
  let -- | Deheapify the given reference and any references it depends on,
      -- adding the deheapfied result to the state in the right order.
      -- Returns an expression to use to represent this.
      deheapr :: ExpR s -> StateT [(ExpR s, Exp)] (ElabH s) Exp
      deheapr r = do
        e <- lift $ readRef r
        case e of
            LitEH l -> return (LitE l)
            VarEH s -> return (VarE s)
            ConEH s -> return (ConE s)
            _ | isfunt (rtype r) -> lift $ inline r
            _ -> do
                done <- get
                case lookup r done of
                    Just _ -> return (rvar r)
                    Nothing -> do
                      e <- lift $ readRef1 r
                      e' <- deheap e
                      modify $ (:) (r, e')
                      return (rvar r)

      deheap :: ExpH s -> StateT [(ExpR s, Exp)] (ElabH s) Exp
      deheap (LitEH l) = return (LitE l)
      deheap (CaseEH x ms) =
        let deheapm (MatchH p b) = do
              b' <- deheapr b 
              return (Match p b')
        in do
            x' <- deheapr x
            ms' <- mapM deheapm ms
            return (CaseE x' ms')
      deheap (AppEH a b) = do
        a' <- deheapr a
        b' <- deheapr b
        return (AppE a' b')
      deheap (LamEH s b) = do
        b' <- deheapr b
        return (LamE s b')
      deheap (ConEH s) = return (ConE s)
      deheap (VarEH s) = return (VarE s)
      deheap (RefEH x) = do
        x' <- deheapr x
        return x'
      deheap (HeapifyEH x) = return x
  in do
     (e, deheaped) <- runStateT (deheapr r) []
     let rbindings = [(rsig r, e) | (r, e) <- deheaped]
     return $ letE (reverse rbindings) e

-- Completely inline all references in the given expression.
-- TODO: Perhaps performance can be improved if we cache inlinings of
-- expressions?
inline :: ExpR s -> ElabH s Exp
inline r = do
    e <- readRef r
    case e of
        LitEH l -> return (LitE l)
        CaseEH x ms ->
            let inlinem (MatchH p b) = do
                    b' <- inline b
                    return (Match p b')
            in do
                x' <- inline x
                ms' <- mapM inlinem ms
                return (CaseE x' ms')
        AppEH a b -> do
            a' <- inline a
            b' <- inline b
            return (AppE a' b')
        LamEH s b -> do
            b' <- inline b
            return (LamE s b')
        ConEH s -> return (ConE s)
        VarEH s -> return (VarE s)
        RefEH {} -> error $ "readRef returned RefEH in inline"
        HeapifyEH x -> return x

    
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
    r <- heapifyr Map.empty e
    elabH [] r
    mode <- gets es_mode
    case mode of
        WHNF -> inline r
        SNF -> deheapify r

elaborateST :: Mode -> Env -> Exp -> ST s Exp
elaborateST mode env e = evalStateT (elaborateH e) (ES env mode Map.empty 1 Map.empty)

elaborate :: Mode -> Env -> Exp -> Exp
elaborate mode env e =
  --runST $ elaborateST mode env e
  let elabed = runST $ elaborateST mode env (trace ("elab " ++ show mode ++ ": " ++ pretty e) e)
  in trace ("elabed: " ++ pretty elabed) elabed

letEH :: [(Sig, ExpR s)] -> ExpR s -> ElabH s (ExpR s)
letEH [] x = return x
letEH ((s, v):bs) x = do
    sub <- letEH bs x
    r <- mkRef (arrowsT [rtype v, rtype x]) $ LamEH s sub
    mkRef (rtype x) (AppEH r v)

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

