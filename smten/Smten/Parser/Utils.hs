
module Smten.Parser.Utils (
    PDec(..), LDec(..), CDec(..),
    coalesce, icoalesce, lcoalesce, ccoalesce,
    mkContext,
 ) where

import Data.Maybe(fromMaybe)

import Smten.Location
import Smten.Failable
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
  | PSig Location TopSig
  | PClause Name MAlt
  | PSynonym Synonym
  | PDeriving Deriving
    deriving (Show)

data CDec = CSig Location TopSig
          | CClause Name MAlt

data LDec =
    LPat Pat Exp
  | LClause Location Name MAlt

isPClause :: PDec -> Bool
isPClause (PClause {}) = True
isPClause _ = False

isCClause :: CDec -> Bool
isCClause (CClause {}) = True
isCClause _ = False

coalesce :: [PDec] -> ([Synonym], [DataDec], [Deriving], [Dec])
coalesce [] = ([], [], [], [])
coalesce ((PSig l s):ds) =
    let (ms, rds) = span isPClause ds
        (syns, dds, drv, rest) = coalesce rds
        d = case ms of
                [] -> PrimD l s
                _ -> ValD l (TopExp s (clauseE l [c | PClause _ c <- ms]))
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
icoalesce :: [(Name, Location, MAlt)] -> [Method]
icoalesce [] = []
icoalesce ((n, l, c):ms) =
    let (me, rms) = span (\(n', _, _) -> n' == n) ms
        rest = icoalesce rms
        m = Method n (clauseE l (c : map (\(_, _, x) -> x) me))
    in (m : rest)

ccoalesce :: [CDec] -> [TopExp]
ccoalesce [] = []
ccoalesce ((CSig l s):ds) =
  let (ms, rds) = span isCClause ds
      rest = ccoalesce rds
      d = case ms of
              [] -> nodefault l s
              _ -> TopExp s (clauseE l [c | CClause _ c <- ms])
  in d:rest
ccoalesce (CClause {} : _) = error $ "SMTEN TODO: handle unexpected CClause in ccoalesce"

lcoalesce :: [LDec] -> [(Pat, Exp)]
lcoalesce [] = []
lcoalesce (LPat p e : ls) = (p, e) : lcoalesce ls
lcoalesce (LClause l n c : ls) =
    let isn :: LDec -> Bool
        isn (LClause _ n' _) = n' == n
        isn (LPat {}) = False

        (me, rms) = span isn ls
        rest = lcoalesce rms
        m = (VarP n, clauseE l (c : [c' | LClause _ _ c' <- me]))
    in (m : rest)

-- A context is parsed first as a type to avoid a reduce/reduce conflict. Here
-- we turn that type back into a proper context.
mkContext :: Type -> ParserMonad [Class] 
mkContext t = 
  let mkclass :: Type -> ParserMonad Class
      mkclass t =
        case de_appsT t of
          (ConT nm _, ts) -> return $ Class nm ts
          _ -> lthrow "invalid context"

      classes = fromMaybe [t] (de_tupleT t)
  in mapM mkclass classes

