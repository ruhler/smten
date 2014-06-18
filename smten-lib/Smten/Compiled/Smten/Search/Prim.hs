
{-# LANGUAGE PatternGuards #-}

module Smten.Compiled.Smten.Search.Prim (
    Space, Solver,
    single, bind, search,
    empty, union,
    free_Integer, free_Bit,
    ) where

import Prelude as P

import Smten.Runtime.FreeID
import Smten.Runtime.Formula
import Smten.Runtime.Formula.Finite
import Smten.Runtime.SmtenHS
import Smten.Runtime.Solver
import Smten.Runtime.Model

import qualified Smten.Compiled.Data.Maybe as S
import Smten.Compiled.GHC.TypeLits

data DT = DTEmpty | DTSingle FreeID | DTSplit DT DT | DTChoice FreeID DT DT

data Space a = Space {
    -- Run the symbolic computation, producing an SMT formula and symbolic
    -- representation of the result.
    -- This takes a unit argument to ensure each time it is run, different
    -- fresh id's are allocated.
    runS :: () -> (BoolF, a, DT),

    -- Realize the symbolic computation given a model determining which
    -- branches to take.
    relS :: Model -> DT -> a
}

instance Functor Space where
    fmap f x = Space {
        runS = \u -> case runS x u of
                        (p, v, t) -> (p, f v, t),
        relS = \m t -> f (relS x m t)
    }
    

instance SmtenHS1 Space where
    ite1 p a b = Space {
        runS = \u -> 
          let (pa, va, ta) = runS a u
              (pb, vb, tb) = runS b u
          in (ite0 p pa pb, ite0 p va vb, DTSplit ta tb),

        relS = \m (DTSplit ta tb) -> ite0 p (relS a m ta) (relS b m tb)
      }

    unreachable1 = Space {
        runS = const (unreachable, unreachable, DTEmpty),
        relS = error "Space.unreachable reached"
    }

single :: a -> Space a
single x = Space {
    runS = const (trueF, x, DTEmpty),
    relS = \m _ -> x
}

bind :: Space a -> (a -> Space b) -> Space b
bind x f = Space {
    runS = \u ->
       let (px, vx, tx) = runS x u
           (pf, vf, tf) = runS (f vx) u
       in (px `andF` pf, vf, DTSplit tx tf),
    relS = \m (DTSplit tx tf) -> relS (f (relS x m tx)) m tf
 }
       

empty :: (SmtenHS0 a) => Space a
empty = Space {
    runS = const (falseF, unreachable, DTEmpty),
    relS = \m _ -> error "Space.relS.empty reached"
 }

union :: (SmtenHS0 a) => Space a -> Space a -> Space a
union a b = Space {
    runS = \u -> withfresh $ \nm ->
        let p = varF nm
            (pa, va, ta) = runS a u
            (pb, vb, tb) = runS b u
        in (ite0 p pa pb, ite0 p va vb, DTChoice nm ta tb),

    relS = \m (DTChoice nm ta tb) ->
        if lookupBool m nm
            then relS a m ta
            else relS b m tb
 }

free_Integer :: Space IntegerF
free_Integer = Space {
    runS = \u -> {-# SCC "freeIntegerS" #-} withfresh $ \nm -> 
        u `seq` (trueF, var_IntegerF nm, DTSingle nm),
    relS = \m (DTSingle nm) -> integerF $ lookupInteger m nm
 }


free_Bit :: SingI Nat n -> Space (BitF n)
free_Bit w = Space {
    runS = \u -> {-# SCC "freeBitS" #-} withfresh $ \nm ->
            u `seq` (trueF, var_BitF (__deNewTyDGSingI w) nm, DTSingle nm),

    relS = \m (DTSingle nm) ->
        bitF $ lookupBit m (__deNewTyDGSingI w) nm
 }


search :: (SmtenHS0 a) => Solver -> Space a -> IO (S.Maybe a)
search s q = do
  case ({-# SCC "RunS" #-} runS q ()) of
     (p, x, t) | (pp, pa, pb) <- deBoolF p -> do
       -- Try to find a solution in the finite part of the formula.
       ares <- {-# SCC "Solve" #-} solve s (pp `andFF` pa)
       case ares of
         Just m -> return (S.__Just ({-# SCC "Realize" #-} relS q m t))
         Nothing -> do
            -- There was no solution found in the finite part.
            -- Check if we have to evaluate the other part.
            bres <- {-# SCC "SolveApprox" #-} solve s (notFF pp)
            case bres of
               Nothing -> return S.__Nothing
               Just _ -> search s (q { runS = const (andF_ (notFF pp) pb, x, t)})

