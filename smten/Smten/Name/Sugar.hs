
module Smten.Name.Sugar (
    arrowN, unitN, trueN, falseN,
    tupleN, de_tupleN,
    ) where

import Smten.Name.Name

import Control.Monad(guard)
import Data.List(genericReplicate, genericLength)

-- | The name of the (->) type constructor.
arrowN :: Name
arrowN = name "->"

unitN :: Name
unitN = name "()"

trueN :: Name
trueN = name "True"

falseN :: Name
falseN = name "False"

-- Generate the tuple name for given number of arguments.
tupleN :: (Integral n) => n -> Name
tupleN n = name $ "(" ++ genericReplicate (n-1) ',' ++ ")"

-- Check if a name is a tuple name. If so, returns the number of elements in
-- the tuple.
de_tupleN :: Name -> Maybe Integer
de_tupleN n = do
    let s = unname n
    guard $ length s > 2
    guard $ head s == '('
    guard $ last s == ')'
    let mid = init (tail s)
    guard $ all (== ',') mid
    return (genericLength mid + 1)

