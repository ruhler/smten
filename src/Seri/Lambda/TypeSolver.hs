
module Seri.Lambda.TypeSolver (solve) where

import Control.Monad.State

import Seri.Failable
import Seri.Lambda.IR

-- | Solve a type constraint system.
-- Here's how we solve it:
--    We define an order on Types based on how well known they are. So
--    IntegerT, ConT, etc... are very well known. VarT less so, and we say
--    VarT 4 is less known than VarT 1, for instance.
--
--    For any constraint of the form X = Y, we use that to replace every
--    occurence of the less well known type with the more well known type. For
--    example, say Y is less well known. Every occurence of Y in all the
--    constraints is replaced with X, and we add Y = X to the solution set.
--
--    The claim is, after going through each constraint, we are left with
--    the best known definitions of each lesser known type we can find.
--
--    The solution set is returned.
--
--  Fails if the constraints are inconsistent.
solve :: [(Type, Type)] -> Failable [(Type, Type)]
solve xs = return . fst $ runState finish (xs, [])

type Solver = State ([(Type, Type)], [(Type, Type)])

finish :: Solver [(Type, Type)]
finish = do
    (sys, sol) <- get
    case sys of
        [] -> return sol
        (x:xs) -> do
            put (xs, sol)
            single x
            finish

-- Solve a single constraint
--  Updates the current system and solution.
single :: (Type, Type) -> Solver ()
single (x, y) | x == y = return ()
single (AppT a b, AppT c d) = single (a, c) >> single (b, d)
single (a, b) | b `lessknown` a = single (b, a)
single (a, b) = do
    (sys, sol) <- get
    let sys' = map (tpreplace a b) sys
    let sol' = map (tpreplace a b) sol
    put (sys', (a,b):sol')

tpreplace :: Type -> Type -> (Type, Type) -> (Type, Type)
tpreplace k v (a, b) = (treplace k v a, treplace k v b)

-- treplace k v x
-- Replace every occurence of type k in x with type v.
treplace :: Type -> Type -> Type -> Type
treplace k v x | k == x = v
treplace k v (AppT a b) = AppT (treplace k v a) (treplace k v b)
treplace _ _ x = x 

lessknown :: Type -> Type -> Bool
lessknown (VarT a) (VarT b) = a > b
lessknown (VarT _) _ = True
lessknown a b = False

