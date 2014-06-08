
{-# LANGUAGE PatternGuards #-}

module Smten.Compiled.Smten.Search.Prim (
    Symbolic, Solver,
    return_symbolic, bind_symbolic, run_symbolic,
    mzero_symbolic, mplus_symbolic,
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

data Symbolic a = Symbolic {
    -- Run the symbolic computation, producing an SMT formula and symbolic
    -- representation of the result.
    -- This takes a unit argument to ensure each time it is run, different
    -- fresh id's are allocated.
    runS :: () -> (BoolF, a, DT),

    -- Realize the symbolic computation given a model determining which
    -- branches to take.
    relS :: Model -> DT -> a
}

instance Functor Symbolic where
    fmap f x = Symbolic {
        runS = \u -> case runS x u of
                        (p, v, t) -> (p, f v, t),
        relS = \m t -> f (relS x m t)
    }
    

instance SmtenHS1 Symbolic where
    ite1 p a b = Symbolic {
        runS = \u -> 
          let (pa, va, ta) = runS a u
              (pb, vb, tb) = runS b u
          in (ite p pa pb, ite p va vb, DTSplit ta tb),

        relS = \m (DTSplit ta tb) -> ite p (relS a m ta) (relS b m tb)
      }

    unreachable1 = Symbolic {
        runS = const (unreachable, unreachable, DTEmpty),
        relS = error "Symbolic.unreachable reached"
    }

return_symbolic :: a -> Symbolic a
return_symbolic x = Symbolic {
    runS = {-# SCC "returnS" #-} const (trueF, x, DTEmpty),
    relS = \m _ -> x
}

bind_symbolic :: Symbolic a -> (a -> Symbolic b) -> Symbolic b
bind_symbolic x f = Symbolic {
    runS = \u ->
       let (px, vx, tx) = runS x u
           (pf, vf, tf) = runS (f vx) u
       in (px `andF` pf, vf, DTSplit tx tf),
    relS = \m (DTSplit tx tf) -> relS (f (relS x m tx)) m tf
 }
       

mzero_symbolic :: (SmtenHS0 a) => Symbolic a
mzero_symbolic = Symbolic {
    runS = {-# SCC "mzeroS" #-} const (falseF, unreachable, DTEmpty),
    relS = \m _ -> error "Symbolic.relS.mzero reached"
 }

mplus_symbolic :: (SmtenHS0 a) => Symbolic a -> Symbolic a -> Symbolic a
mplus_symbolic a b = Symbolic {
    runS = \u -> withfresh $ \nm ->
        let p = varF nm
            (pa, va, ta) = runS a u
            (pb, vb, tb) = runS b u
        in (ite p pa pb, ite p va vb, DTChoice nm ta tb),

    relS = \m (DTChoice nm ta tb) ->
        if lookupBool m nm
            then relS a m ta
            else relS b m tb
 }

free_Integer :: Symbolic IntegerF
free_Integer = Symbolic {
    runS = \u -> {-# SCC "freeIntegerS" #-} withfresh $ \nm -> 
        u `seq` (trueF, var_IntegerF nm, DTSingle nm),
    relS = \m (DTSingle nm) -> integerF $ lookupInteger m nm
 }


free_Bit :: SingI Nat n -> Symbolic (BitF n)
free_Bit w = Symbolic {
    runS = \u -> {-# SCC "freeBitS" #-} withfresh $ \nm ->
            u `seq` (trueF, var_BitF (__deNewTyDGSingI w) nm, DTSingle nm),

    relS = \m (DTSingle nm) ->
        bitF $ lookupBit m (__deNewTyDGSingI w) nm
 }


run_symbolic :: (SmtenHS0 a) => Solver -> Symbolic a -> IO (S.Maybe a)
run_symbolic s q = do
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
               Just _ -> run_symbolic s (q { runS = const (andF_ (notFF pp) pb, x, t)})

