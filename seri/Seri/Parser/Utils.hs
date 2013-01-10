
module Seri.Parser.Utils (
    PDec(..), LDec(..),
    coalesce, icoalesce, lcoalesce,
    mkContext,
 ) where

import Data.Maybe(fromMaybe)

import Seri.Sig
import Seri.Name
import Seri.Type
import Seri.Exp
import Seri.Dec
import Seri.Module
import Seri.Parser.Monad

data PDec =
    PDec Dec
  | PSig TopSig
  | PClause Name MMatch
  | PSynonym Synonym

data LDec =
    LPat Pat Exp
  | LClause Name MMatch

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: [PDec] -> ([Synonym], [Dec])
coalesce [] = ([], [])
coalesce ((PSig s):ds) =
    let (ms, rds) = span isPClause ds
        (syns, rest) = coalesce rds
        d = case ms of
                [] -> PrimD s
                _ -> ValD s (clauseE [c | PClause _ c <- ms]) 
    in (syns, d:rest)
coalesce ((PDec d):ds) =
   let (syns, rest) = coalesce ds
   in (syns, d:rest)
coalesce ((PSynonym s):ds) =
   let (syns, rest) = coalesce ds
   in (s:syns, rest)

-- Merge clauses for the same method into a single method.
icoalesce :: [(Name, MMatch)] -> [Method]
icoalesce [] = []
icoalesce ((n, c):ms) =
    let (me, rms) = span (\(n', _) -> n' == n) ms
        rest = icoalesce rms
        m = Method n (clauseE (c : map snd me))
    in (m : rest)

lcoalesce :: [LDec] -> [(Pat, Exp)]
lcoalesce [] = []
lcoalesce (LPat p e : ls) = (p, e) : lcoalesce ls
lcoalesce (LClause n c : ls) =
    let isn :: LDec -> Bool
        isn (LClause n' _) = n' == n
        isn (LPat {}) = False

        (me, rms) = span isn ls
        rest = lcoalesce rms
        m = (VarP n, clauseE (c : [c' | LClause _ c' <- me]))
    in (m : rest)

-- A context is parsed first as a type to avoid a reduce/reduce conflict. Here
-- we turn that type back into a proper context.
mkContext :: Type -> ParserMonad [Class] 
mkContext t = 
  let mkclass :: Type -> ParserMonad Class
      mkclass t =
        case de_appsT t of
          (ConT nm, ts) -> return $ Class nm ts
          _ -> lfailE "invalid context"

      classes = fromMaybe [t] (de_tupleT t)
  in mapM mkclass classes

