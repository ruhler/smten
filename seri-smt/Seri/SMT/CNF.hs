
module Seri.SMT.CNF (
  SATFormula(..),
  CNFFormula(..), CNFClause(..), CNFLiteral(..),
  smt2sat, cnfify, sat2cnf, smt2cnf,
  ) where

import Control.Monad.Error
import Seri.SMT.Syntax

data SATFormula =
   VarSF String
 | NotSF SATFormula
 | AndSF [SATFormula]
 | OrSF [SATFormula]
deriving (Eq, Show)

isand :: SATFormula -> Bool
isand (AndSF {}) = True
isand _ = False

unor :: SATFormula -> [SATFormula]
unor (OrSF xs) = xs
unor x = [x]

data CNFLiteral = CNFLiteral {
    cnfl_positive :: Bool,
    cnfl_name :: String
} deriving (Eq, Show)

newtype CNFClause = CNFClause [CNFLiteral]
  deriving (Eq, Show)

newtype CNFFormula = CNFFormula [CNFClause]
  deriving (Eq, Show)

-- | Convert an SMT expression to a SAT formula.
-- Fails if there are constructs which can't be converted to SAT.
smt2sat :: (Functor m, MonadError String m) => Expression -> m SATFormula
smt2sat e | Just nm <- de_varE = return $ VarSF nm
smt2sat e | Just (a, b) <- de_eqE e = do
    a' <- smt2sat a
    b' <- smt2sat b
    return $ OrSF [AndSF [a', b'], AndSF [NotSF a', NotSF b']]
smt2sat e | Just (p, a, b) <- de_ifE e = do
    p' <- smt2sat p
    a' <- smt2sat a
    b' <- smt2sat b
    return $ OrE [AndSF [p', a'], AndSF [NotSF p', b']]
smt2sat e | Just a <- de_notE e = NotSF <$> smt2sat a
smt2sat e | Just xs <- de_andE e = AndSF <$> mapM smt2sat xs
smt2sat e | Just xs <- de_orE e = OrSF <$> mapM smt2sat xs
smt2sat e = throwError $ "Unsupported SAT construct: " ++ show e

-- | Given a SAT formula, reduce it to conjunctive normal form.
cnfify :: SATFormula -> SATFormula
cnfify =
  let -- Push all NOT applications down to literals
      deNot :: SATFormula -> SATFormula
      deNot x@(VarSF {}) = x
      deNot (NotSF (NotSF x)) = deNot x
      deNot (NotSF (AndSF xs)) = deNot (OrSF (map NotSF xs))
      deNot (NotSF (OrSF xs)) = deNot (AndSF (map NotSF xs))
      deNot (AndSF xs) = AndSF (map deNot xs)
      deNot (OrSF xs) = OrSF (map deNot xs)

      -- Push all OR applications down as needed
      deOr :: SATFormula -> SATFormula
      deOr x@(VarSF {}) = x
      deOr x@(NotSF {}) = x
      deOr (AndSF xs) = AndSF (map deOr xs)
      deOr (OrSF xs) = 
        let xs' = map deOr xs
            (ands, rest) = partition isand xs'
        in case ands of
             [] -> OrSF (concat $ map unor xs)
             (AndSF as : r) -> AndSF (map (\a -> deOr (OrSF $ (a:r) ++ rest)) as)
  in deOr . deNot


-- | Convert a SAT formula to a CNF formula.
-- It is an error if the SAT formula is not already in conjunctive normal
-- form.
sat2cnf :: SATFormula -> CNFFormula
sat2cnf (AndSF xs) = CNFFormula (map sat2clause xs)
sat2cnf x = CNFFormula [sat2clause x]

sat2clause :: SATFormula -> CNFClause
sat2clause (OrSF xs) = CNFClause (map sat2literal xs)
sat2clause x@(AndSF {}) = error $ "sat2clause: got " ++ show x
sat2clause x = CNFClause [sat2literal x]

sat2literal :: SATFormula -> CNFLiteral
sat2literal (VarSF nm)
  = CNFLiteral { cnfl_positive = True, cnfl_name = nm }
sat2literal (NotSF (VarSF nm))
  = CNFLiteral { cnfl_positive = Flase, cnfl_name = nm }
sat2literal x = error $ "sat2literal: got " ++ show x

-- | Convert an SMT formula to a CNF formula.
-- Fails of there are constructs which can't be converted to SAT.
-- Performs reduction to CNF form.
smt2cnf :: (Functor m, MonadError String m) => Expression -> m CNFFormula
smt2cnf e = sat2cnf <$> smt2sat e

