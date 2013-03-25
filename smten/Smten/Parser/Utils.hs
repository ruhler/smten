
module Smten.Parser.Utils (
    PDec(..), LDec(..),
    coalesce, icoalesce, lcoalesce,
    mkContext,
 ) where

import Data.Maybe(fromMaybe)

import Smten.Sig
import Smten.Name
import Smten.Type
import Smten.Exp
import Smten.Dec
import Smten.Module
import Smten.Parser.Monad

data PDec =
    PDec Dec
  | PDataDec DataDec
  | PSig TopSig
  | PClause Name MAlt
  | PSynonym Synonym
  | PDeriving Deriving
    deriving (Show)

data LDec =
    LPat Pat Exp
  | LClause Name MAlt

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

coalesce :: [PDec] -> ([Synonym], [DataDec], [Deriving], [Dec])
coalesce [] = ([], [], [], [])
coalesce ((PSig s):ds) =
    let (ms, rds) = span isPClause ds
        (syns, dds, drv, rest) = coalesce rds
        d = case ms of
                [] -> PrimD s
                _ -> ValD (TopExp s (clauseE [c | PClause _ c <- ms]))
    in (syns, dds, drv, d:rest)
coalesce ((PDec d):ds) =
   let (syns, dds, drv, rest) = coalesce ds
   in (syns, dds, drv, d:rest)
coalesce ((PDataDec dd):ds) = 
   let (syns, rest, drv, d) = coalesce ds
   in (syns, dd:rest, drv, d)
coalesce ((PSynonym s):ds) =
   let (syns, dds, drv, rest) = coalesce ds
   in (s:syns, dds, drv, rest)
coalesce ((PDeriving d):ds) =
   let (syns, dds, drv, rest) = coalesce ds
   in (syns, dds, d:drv, rest)
coalesce (p@(PClause {}) : ds) = error $ "coalesce: TODO: handle unexpected PClause: " ++ show p


-- Merge clauses for the same method into a single method.
icoalesce :: [(Name, MAlt)] -> [Method]
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
          (ConT nm _, ts) -> return $ Class nm ts
          _ -> lfailE "invalid context"

      classes = fromMaybe [t] (de_tupleT t)
  in mapM mkclass classes

