
module Smten.Exp.Do (
    Stmt(..), doE,
    ) where

import Smten.Type
import Smten.Name
import Smten.Sig
import Smten.Fresh
import Smten.Exp.Exp
import Smten.Exp.Match
import Smten.Exp.Sugar

data Stmt = 
    BindS Pat Exp   -- ^ n <- e
  | NoBindS Exp     -- ^ e
  | LetS [(Pat, Exp )]   -- ^ let p = e
    deriving(Eq, Show)

-- | do { stmts }
-- The final statement of the 'do' must be a NoBindS.
doE :: [Stmt] -> Exp
doE [] = error $ "doE on empty list"
doE [NoBindS e] = e 
doE ((LetS bs):stmts) =
  let rest = doE stmts
  in mletsE bs rest
doE ((NoBindS e):stmts) =
  let rest = doE stmts
      tbind = (arrowsT [typeof e, typeof rest, typeof rest])
  in appsE (varE (Sig (name ">>") tbind)) [e, rest]
doE ((BindS p e):stmts) =
  let rest = doE stmts
      f = mlamE [p] rest
      tbind = (arrowsT [typeof e, typeof f, typeof rest])
  in appsE (varE (Sig (name ">>=") tbind)) [e, f]


