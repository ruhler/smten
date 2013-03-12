
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

module Smten.SMT.Symbolic (
    Used(..), Context, Contexts,
    Symbolic, runSymbolic,
    prim_free, assert, used,
    predicated,
    de_symbolicEH,
    ) where

import Control.Monad.State

import Data.Typeable
import Data.Unique

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.Lit
import Smten.Ppr
import Smten.ExpH

import Smten.SMT.IVP

type Context = Unique
type Contexts = [Unique]

data Used a = Used Context a
    deriving (Typeable)

instance Functor Used where
    fmap f (Used ctx a) = Used ctx (f a)

data SS = SS {
    -- | Stack of currently active contexts
    ss_ctx :: Contexts,

    -- | Next free id to use for a free variables.
    ss_nfree :: Integer,

    -- | List of free variables created so far (in reverse order)
    ss_free :: [Sig],

    -- | assertions made (in reverse order).
    ss_asserts :: [ExpH]
}

newtype Symbolic a = Symbolic {
    symbolic_state :: State SS a
} deriving (Functor, Monad, Typeable)

deriving instance MonadState SS Symbolic

-- | Assert the given predicate.
assert :: ExpH -> Symbolic ()
assert p = modify $ \ss -> ss { ss_asserts = p : ss_asserts ss }

-- | Read the value of a Used.
used :: Used a -> Symbolic a
used (Used ctx v) = do
    ctxs <- gets ss_ctx
    if (ctx `elem` ctxs)
       then return $ v
       else error "used of ref in invalid context"

-- | Allocate a primitive free variable of the given type.
-- The underlying SMT solver must support this type for this to work.
prim_free :: Type -> Symbolic ExpH
prim_free t = do
    fid <- gets ss_nfree
    let f = Sig (name $ "free~" ++ show fid) t
    modify $ \ss -> ss { ss_nfree = fid+1, ss_free = f : ss_free ss }
    return (varEH f)

-- | Predicate the symbolic computation on the given smten Bool.
-- All assertions will only apply when the predicate is satisfied, otherwise
-- the assertions become vacuous.
predicated :: ExpH -> Symbolic a -> Symbolic a
predicated p s = do
    ss <- get
    let (v, ss') = runState (symbolic_state s) (ss { ss_asserts = [] })
        ssa' = [impliesEH p q | q <- ss_asserts ss']
    put (ss' { ss_asserts = ssa' ++ ss_asserts ss })
    return v

-- | Run a Symbolic computation.
-- Inputs:
--   ctx - the stack of currently active contexts
--   nfree - id to use for next free variable
--   s - the symbolic thing to run
-- Outputs: (nfree', free, asserts, val)
--   nfree' - the new id to use for next free variables
--   free - the primitive free variables created (in order of creation)
--   asserts - the assertions made (of type Bool) (in order of assertion)
--   val - the result of the computation.
runSymbolic :: Contexts -> Integer -> Symbolic a -> (Integer, [Sig], [ExpH], a)
runSymbolic ctx nfree s = 
    let (v, ss) = runState (symbolic_state s) (SS ctx nfree [] [])
    in (ss_nfree ss, reverse (ss_free ss), reverse (ss_asserts ss), v)

-- Convert an ExpH of smten type (Symbolic a) to it's corresponding haskell
-- Symbolic.
--
-- This assumes you are passing an expression of seri type (Symbolic a).
-- It will always succeed, because it automatically converts symbolic
-- (Symbolic a) into concrete (Symbolic a)
de_symbolicEH :: ExpH -> Symbolic ExpH
de_symbolicEH e
 | Just l <- de_litEH e, Just s <- de_dynamicL l = s
 | CaseEH _ x k y n <- ivp e =  -- TODO: use of ivp here is maybe a hack. Fix it.
    let yv = if 0 == length (de_arrowsT (typeof k)) - 1
                then y
                else -- TODO: Can this ever occur?
                     -- I think not, because caseEH gets rid of any case
                     -- expression on things other than booleans.
                     error $ "de_symbolicEH of complex case expressions in: " ++ pretty e
        py = tcaseEH e (const trueEH) (const falseEH)
        pn = tcaseEH e (const falseEH) (const trueEH)

        ys = de_symbolicEH yv
        ns = de_symbolicEH n
    in do
        yr <- predicated py ys
        nr <- predicated pn ns
        return $ tcaseEH e (const yr) (const nr)
 | ErrorEH _ msg <- e = error $ "(de_symbolicEH): " ++ msg
 | otherwise = error $ "de_symbolicEH: " ++ pretty e

