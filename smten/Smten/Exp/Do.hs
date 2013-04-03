
module Smten.Exp.Do (
    Stmt(..), doE,
    ) where

import Smten.Location
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
doE :: Location -> [Stmt] -> Exp
doE _ [] = error $ "doE on empty list"
doE _ [NoBindS e] = e 
doE l ((LetS bs):stmts) =
  let rest = doE l stmts
  in mletsE l bs rest
doE l ((NoBindS e):stmts) =
  let rest = doE l stmts
      tbind = (arrowsT [typeof e, typeof rest, typeof rest])
  in appsE l (varE l (Sig (name ">>") tbind)) [e, rest]
doE l ((BindS p e):stmts) =
  let rest = doE l stmts
      f = mlamE l [p] rest
      tbind = (arrowsT [typeof e, typeof f, typeof rest])
  in appsE l (varE l (Sig (name ">>=") tbind)) [e, f]


