
module Smten.Runtime.Trace (traceSD) where

import Smten.Runtime.Formula.BoolF

-- Helper function for implementing traceS for algebraic data types.
-- Takes: A list of constructors: their name, guards and traced fields.
traceSD :: [(String, BoolF, [String])] -> String
traceSD xs =
  let targs args = concatMap ((" (" ++) . (++ ")")) args
  in case filter (\(_, p, _) -> not (isFalseF p)) xs of
        [(s,_,args)] -> s ++ targs args
        xs' -> 
          let f (s,p,args) = s ++ " [" ++ trace_BoolF p ++ "]" ++ targs args
          in unlines (map f xs')

