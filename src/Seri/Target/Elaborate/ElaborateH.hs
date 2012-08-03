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

-- | Simple elaboration does as little elaboration as possible once it's clear
-- progress can't be made.
--
-- Full elaboration fully elaborates everything.
-- A fully elaborated expression is potentially an infinite object.
data Mode = Simple | Full
    deriving (Show, Eq)

data ES = ES {
    es_env :: Env,
    es_mode :: Mode,

    -- Declared variables already looked up
    es_decls :: Map.Map Sig ExpR,

    -- next available unique id to use
    es_nid :: ID,

    -- List of bound variables in scope
    es_bound :: [Name]

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

data ExpH s
  = LitEH Lit
  | CaseEH (ExpR s) [MatchH s]
  | AppEH (ExpR s) (ExpR s)
  | LamEH Sig (ExpR s)
  | ConEH Sig
  | VarEH Sig

data MatchH s = MatchH Pat (ExpR s)

-- | Make and return a reference to the given expression.
mkRef :: ExpH s -> ElabH (ExpR s)
mkRef e = do
    id <- gets es_nid
    modify $ \es -> es { es_nid = id+1 }
    r <- liftST $ newSTRef (e, Nothing)
    return $ ExpR id r

readRef :: ExpR -> ElabH ExpH
readRef r = fmap fst $ liftST $ readSTRef r

writeRef :: ExpR -> ExpH -> ElabH ()
writeRef r e = liftST $ writeSTRef r (e, Nothing)
    
readReachable :: ExpR -> ElabH (Maybe (Set.Set ExpR))
readReachable r = fmap snd $ liftST $ readSTRef r

withbound :: Name -> ELabH a -> ElabH a
withbound n = withbounds [n]

withbounds :: [Name] -> ELabH a -> ElabH a
withbounds ns x = do
    bound <- gets es_bound
    modify $ \es -> es { es_bound = ns ++ bound }
    r <- x
    modify $ \es -> es { es_bound = bound }
    return r

isbound :: Name -> ElabH Bool
isbound n = do
    bound <- gets es_bound
    return (n `elem` bound)

-- | Given the signature of a declared variable, return a reference to its
-- value.
declared :: Sig -> ElabH (ExpR s)
declared s@(Sig _ ct) = do
    env <- gets es_env
    decls <- gets es_decls
    case Map.lookup s decls of
        Just x -> return x
        Nothing -> do
            case (attemptM $ lookupVar env s) of
               Nothing -> return (VarEH s)
               Just (pt, ve) -> do
                  veh <- heapify $ assign (assignments pt ct) ve
                  modify $ \es -> es { es_decls = Map.insert s veh (es_decls es) }
                  return veh

heapify :: Exp -> ElabH ExpR
heapfiy (LitE l) = mkRef (LitEH l)
heapfiy (CaseE x ms) =
 let hm (Match p b) = do
        b' <- withbounds (bindingsP' p) $ heapify b
        return (MatchH p b')
 in do
    x' <- heapify x
    ms' <- mapM hm ms
    mkRef (CaseEH x' ms')
heapfiy (AppE a b) = do
    a' <- heapify a
    b' <- heapify b
    mkRef (AppEH a' b')
heapify (LamE s@(Sig n t) b) = do
    b' <- withbound n $ heapify b
    mkRef (LamEH s b')
heapify (ConE s) = return (ConEH s)
heapify (VarE s@(Sig n _)) = do
    bound <- isbound n
    if bound
        then mkRef (VarEH s)
        else declared s

-- Cause the expression the given reference refers to be elaborated.
elabH :: ExpR -> ElabH ()
ealbH r = do
  mode <- gets es_mode
  e <- readRef r
  case e of
    LitEH {} -> return ()
    CaseEH x ms -> do
       mr <- matches ms x
       case mr of
         -- TODO: return error if no alternative matches?
         NoMatched -> writeRef r $ CaseEH x [last ms]
         Matched vs b -> writeRef r (letEH vs b) >> elabH r
         UnMatched ms' | mode == Simple -> writeRef r $ CaseEH x ms'
         UnMatched ms' | mode == Full -> 
            let elabmH (MatchH p b) = elabH b
            in mapM_ elabmH ms'
    AppEH a b -> do
        elabH a
        av <- readRef ar
        case av of
          VarEH (Sig "Seri.Lib.Prelude.valueof" t) ->
             let NumT nt = head $ unarrowsT t
             in writeRef r $ integerEH (nteval nt)
          AppEH p x ->  do -- Handle binary primitives
             pv <- readRef p
             case pv of
                VarEH (Sig n _) | isBinaryPrim n -> do
                   elabH x
                   xv <- readRef x
                   case xv of
                     LitEH la -> do
                        elabH b
                        bv <- readRef b
                        case bv of
                            LitEH lb -> binaryPrim n r la lb
                            _ -> return ()
                     _ -> return ()
          LamEH (Sig n _) body -> do
            body' <- reduce n b body
            writeRef r $ body'


          _ | mode == Full -> elabH b
          _ -> return ()
    LamEH {} | mode == Simple -> return ()
    LamEH (Sig n _) b -> elabH b
    ConEH {} -> return ()
    VarEH (Sig "Seri.Lib.Prelude.numeric" (NumT nt)) ->
        writeRef r $ ConEH (Sig ("#" ++ show (nteval nt)) (NumT nt))
    VarEH {} -> return ()

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

-- reduce n v e
-- Replace occurences of n with v in the expression e.
-- Returns a reference to the new expression with replacements.
reduce :: Name -> ExpR -> ExpR -> ElabH ExpR
reduce n v e = fmap (fromMaybe e) (reduce' n v e)

-- Same as reduce, but returns Nothing if no reductions were made.
reduce' :: Name -> ExpR -> ExpR -> ElabH (Maybe ExpR)
reduce' n v e = do
  ev <- readRef e
  case ev of
    LitEH {} -> return Nothing
    CaseEH x ms ->
      let rm :: MatchH -> ElabH (Maybe MatchH)
          rm (MatchH p b) = do
            b' <- reduce' n v b
            return $ do
                v <- b'
                return (MatchH p v)
      in do
        x' <- reduce' n v b
        ms' <- mapM rm ms 
        if (isNothing x' && all isNothing ms')
          then return Nothing
          else
             let usex = fromMaybe x x'
                 usems = [fromMaybe m m' | (m, m') <- zip ms ms']
             in fmap Just $ mkRef (CaseEH usex usems)
    AppEH a b -> do
        a' <- reduce' n v a
        b' <- reduce' n v b
        case (a', b') of
            (Nothing, Nothing) -> return Nothing
            _ -> fmap Just $ mkRef (AppEH (fromMaybe a a') (fromMaybe b b'))
    LamEH (Sig nm _) _ | nm == n -> return Nothing
    LamEH s b -> do
        b' <- reduce' n v b
        return $ do
            v <- b'
            return (LamEH s v)
    ConEH {} -> return Nothing
    VarEH (Sig nm _) | nm == n -> return (Just v)
    VarEH {} -> return Nothing
        
class Reachable a where
  -- Return the set of complex reference reachable from the given object.
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

instance Reachable MatchH where
  reachable (Match p b) = reachable b

instance Reachable ExpR where
  reachable r =
    let isComplex (CaseEH {}) = True
        isComplex (AppEH {}) = True
        isComplex (LamEH {}) = True
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

-- Given the set of reference which can be assumed to be in scope, deheapify
-- an expression.
deheapify :: Set.Set ExpR -> HExp -> ElabH Exp
deheapify f e =
  case e of
    LitEH l -> return (LitE l)
    CaseEH x ms -> do
        rx <- reachable x
        rms <- reachable ms
        let dohere = Set.difference (Set.intersection rx rms) f
        bindings <- mapM mkBinding (elems dohere)
        let dontdo = Set.union dohere f
        xdh <- deheapify (Set.intersection rx dontdo) x
        msdh <- deheapify (Set.intersection rms dontdo) ms
        return $ letE bindings (CaseE xdh mdsh)
    AppEH a b -> do
        ra <- reachable a
        rb <- reachable b
        let dohere = Set.difference (Set.intersection ra rb) f
        bindings <- mapM mkBinding (elems dohere)
        let dontdo = Set.union dohere f
        adh <- deheapify (Set.intersection ra dontdo) a
        bdh <- deheapify (Set.intersection rb dontdo) b
        return $ letE bindings (AppE adh bdh)
    LamEH s b -> do
        bdh <- deheapify b f
        return $ LamE s bdh
    ConEH s -> return (ConE s)
    VarEH s -> return (VarE s)

elaborateH :: Exp -> ElabH Exp
elaborateH e = do
    heapified <- heapify e
    elabH heapified
    v <- readRef heapified
    deheapify Set.empty v

elaborateST :: Mode -> Env -> Exp -> ST s Exp
elaborateST mode env e = evalStateT (elaborateH e) (ES env mode Map.empty 1 [])

elaborate :: Mode -> Env -> Exp -> Exp
elaborate mode env e = runST $ elaborateST mode env e

